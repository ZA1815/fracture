use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, quote_spanned};
use syn::{
    Expr, Ident, Pat, Token, parse::{Parse, ParseStream}, parse_macro_input, punctuated::Punctuated, spanned::Spanned
};

struct SelectMacro {
    has_biased: bool,
    branches: Vec<Branch>,
    else_branch: Option<ElseBranch>
}

struct Branch {
    pattern: Pat,
    future: Expr,
    guard: Option<Expr>,
    body: Expr,
    branch_id: String
}

struct ElseBranch {
    body: Expr
}

struct Pattern(pub syn::Pat);

impl syn::parse::Parse for Pattern {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        syn::Pat::parse_single(input).map(Pattern)
    }
}

syn::custom_keyword!(biased);

impl Parse for SelectMacro {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut has_biased = false;
        let mut branches = Vec::new();
        let mut else_branch = None;

        if input.peek(biased) {
            input.parse::<biased>()?;
            input.parse::<Token![;]>()?;
            has_biased = true;
        }

        let mut branch_counter = 0;
        while !input.is_empty() {
            if input.peek(Token![else]) {
                input.parse::<Token![else]>()?;
                let body = input.parse()?;
                else_branch = Some(ElseBranch { body });
                break;
            }

                let Pattern(pattern) = input.parse()?;
                input.parse::<Token![=]>()?;
                let future = input.parse()?;

            let guard = if input.peek(Token![if]) {
                input.parse::<Token![if]>()?;
                Some(input.parse()?)
            }
            else {
                None
            };

            input.parse::<Token![=>]>()?;
            let body = input.parse()?;

            branches.push(Branch {
                pattern,
                future,
                guard,
                body,
                branch_id: format!("branch_{}", branch_counter)
            });

            branch_counter += 1;

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(SelectMacro {
            has_biased,
            branches,
            else_branch
        })
    }
}

pub fn select(input: TokenStream) -> TokenStream {
    let select = parse_macro_input!(input as SelectMacro);

    if select.branches.is_empty() && select.else_branch.is_none() {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "select! requires at least one branch"
        )
        .to_compile_error()
        .into();
    }

    let expanded = generate_select(select);
    TokenStream::from(expanded)
}

fn generate_select(select: SelectMacro) -> proc_macro2::TokenStream {
    let SelectMacro {
        has_biased,
        branches,
        else_branch
    } = select;

    let future_idents: Vec<Ident> = branches
        .iter()
        .enumerate()
        .map(|(i, _)| syn::Ident::new(&format!("__fut_{}", i), proc_macro2::Span::call_site()))
        .collect();

    let pin_futures = future_idents.iter().zip(&branches).map(|(ident, branch)| {
        let future = &branch.future;
        quote! {
            let mut #ident = #future;
            ::std::pin::Pin::new(&mut #ident)
        }
    });

    let poll_branches = if has_biased {
        generate_biased_polling(&branches, &future_idents)
    }
    else {
        generate_random_polling(&branches, &future_idents)
    };

    let else_handling = if let Some(else_branch) = else_branch {
        let body = &else_branch.body;
        quote! {
            if __all_pending {
                return #body;
            }
        }
    }
    else {
        quote! {}
    };

    quote! {
        {
            use ::std::task::Poll;
            use ::std::pin::Pin;
            use ::std::future::Future;

            #(#pin_futures;)*

            ::std::future::poll_fn(|__cx| {
                let mut __all_pending = true;

                #poll_branches

                #else_handling

                Poll::Pending
            }).await
        }
    }
}

fn generate_biased_polling(branches: &[Branch], future_idents: &[Ident]) -> proc_macro2::TokenStream {
    let poll_branches = branches.iter().zip(future_idents).map(|(branch, ident)| {
        let pattern = &branch.pattern;
        let body = &branch.body;
        let guard = branch.guard.as_ref().map(|g| quote! { if #g });
        let _branch_id = &branch.branch_id;

        quote_spanned! {
            branch.pattern.span() =>
            if !crate::chaos::should_fail(crate::chaos::ChaosOperation::SelectStarvation) {
                match #ident.as_mut().poll(__cx) {
                    Poll::Ready(__result) => {
                        let #pattern = __result;
                        #guard {
                            return Poll::Ready((|| #body)());
                        }
                    }
                    Poll::Pending => {}
                }
            }
        }
    });

    quote! {
        #(#poll_branches)*

        __all_pending = false;
    }
}

fn generate_random_polling(branches: &[Branch], future_idents: &[Ident]) -> proc_macro2::TokenStream {
    let n = branches.len();

    let indices: Vec<usize> = (0..n).collect();

    let poll_branches = branches.iter().zip(future_idents).enumerate().map(|(_, (branch, ident))| {
        let pattern = &branch.pattern;
        let body = &branch.body;
        let guard = branch.guard.as_ref().map(|g| quote! { if #g });

        quote_spanned! {
            branch.pattern.span() =>
            match #ident.as_mut().poll(__cx) {
                Poll::Ready(__result) => {
                    let #pattern = __result;
                    #guard {
                        return Poll::Ready((|| #body)());
                    }
                }
                Poll::Pending => {}
            }
        }
    });

    quote! {
        let mut __order = [#(#indices), *];

        if crate::chaos::should_fail(crate::chaos::ChaosOperation::SelectRandom) {
            for __i in (1..#n).rev() {
                let __j = (crate::chaos::get_seed() as usize % (__i + 1));
                __order.swap(__i, __j);
            }
        }

        for &__idx in &__order {
            match __idx {
                #(
                    #indices => {
                        #poll_branches
                    }
                )*
                _ => unreachable!()
            }
        }

        __all_pending = false;
    }
}

pub fn join(input: TokenStream) -> proc_macro::TokenStream {
    let futures = parse_macro_input!(input with syn::punctuated::Punctuated::<Expr, Token![,]>::parse_terminated);

    let future_idents: Vec<_> = (0..futures.len())
        .map(|i| syn::Ident::new(&format!("__fut_{}", i), Span::call_site()))
        .collect();

    let pin_futures = futures.iter().zip(&future_idents).map(|(fut, ident)| {
        quote! {
            let mut #ident = #fut;
            // SAFETY: The future is shadowed and cannot be moved after this point
            let mut #ident = unsafe { ::std::pin::Pin::new_unchecked(&mut #ident) };
        }
    });

    let poll_all = future_idents.iter().enumerate().map(|(i, ident)| {
        let result_ident = Ident::new(&format!("__result_{}", i), Span::call_site());
        quote! {
            let #result_ident = if __completed[#i] {
                None
            }
            else {
                match #ident.as_mut().poll(__cx) {
                    ::std::task::Poll::Ready(__r) => {
                        __completed[#i] = true;
                        __remaining -= 1;
                        Some(__r)
                    }
                    ::std::task::Poll::Pending => None
                }
            };
        }
    });

    let result_tuple = future_idents.iter().enumerate().map(|(i, _)| {
        let result_ident = Ident::new(&format!("__result_{}", i), Span::call_site());
        quote! { #result_ident.unwrap() }
    });

    let n = pin_futures.len();

    let expanded = quote! {
        {
            use ::std::task::Poll;
            use ::std::pin::Pin;

            #(#pin_futures)*

            let mut __completed = [false; #n];
            let mut __remaining = #n;

            ::std::future::poll_fn(|__cx| {
                #(#poll_all)*

                if __remaining == 0 {
                    Poll::Ready((#(#result_tuple),*))
                }
                else {
                    Poll::Pending
                }
            }).await
        }
    };

    TokenStream::from(expanded)
}

pub fn try_join(input: TokenStream) -> TokenStream {
    let futures = parse_macro_input!(input with Punctuated::<Expr, Token![,]>::parse_terminated);

    let future_idents: Vec<_> = (0..futures.len())
        .map(|i| Ident::new(&format!("__fut_{}", i), Span::call_site()))
        .collect();

    let pin_futures = futures.iter().zip(&future_idents).map(|(fut, ident)| {
        quote! {
            let mut #ident = #fut;
            // SAFETY: The future is shadowed and cannot be moved after this point
            let mut #ident = unsafe { ::std::pin::Pin::new_unchecked(&mut #ident) };
        }
    });

    let poll_all = future_idents.iter().enumerate().map(|(i, ident)| {
        let result_ident = Ident::new(&format!("__result_{}", i), Span::call_site());
        quote! {
            let #result_ident = if __completed[#i] {
                None
            }
            else {
                match #ident.as_mut().poll(__cx) {
                    ::std::task::Poll::Ready(Ok(__r)) => {
                        __completed[#i] = true;
                        __remaining -= 1;
                        Some(Ok(__r))
                    }
                    ::std::task::Poll::Ready(Err(__e)) => {
                        return ::std::task::Poll::Ready(Err(__e.into()));
                    }
                    ::std::task::Poll::Pending => None
                }
            };
        }
    });

    let result_tuple = future_idents.iter().enumerate().map(|(i, _)| {
        let result_ident = Ident::new(&format!("__result_{}", i), Span::call_site());
        quote! { #result_ident.unwrap().unwrap() }
    });

    let n = pin_futures.len();

    let expanded = quote! {
        {
            use ::std::task::Poll;
            use ::std::pin::Pin;

            #(#pin_futures)*

            let mut __completed = [false; #n];
            let mut __remaining = #n;

            ::std::future::poll_fn(|__cx| {
                #(#poll_all)*

                if __remaining == 0 {
                    Poll::Ready(Ok((#(#result_tuple),*)))
                }
                else {
                    Poll::Pending
                }
            }).await
        }
    };

    TokenStream::from(expanded)
}

pub fn pin(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Expr);

    let expanded = quote! {
        {
            let mut __pinned = #input;
            let mut __pinned = unsafe {
                ::std::pin::Pin::new_unchecked(&mut __pinned)
            };
            __pinned
        }
    };

    TokenStream::from(expanded)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_select_parse() {
        let input = quote! {
            _ = async { 1 } => { println!("one"); },
            _ = async { 2 } => { println!("two"); },
        };

        let parsed = syn::parse2::<SelectMacro>(input);
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_biased_select() {
        let input = quote! {
            biased;
            _ = async { 1 } => { println!("one"); },
            _ = async { 2 } => { println!("two"); },
        };

        let parsed = syn::parse2::<SelectMacro>(input).unwrap();
        assert!(parsed.has_biased);
    }
}