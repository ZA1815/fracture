use proc_macro::TokenStream;
use quote::quote;
use syn::{Expr, ExprLit, ItemFn, Lit, Meta, ReturnType, parse_macro_input, punctuated::Punctuated, token::Comma, Token};

struct MacroArgs {
    duration: proc_macro2::TokenStream
}

impl Default for MacroArgs {
    fn default() -> Self {
        Self {
            duration: quote! { ::std::time::Duration::from_secs(60) }
        }
    }
}

impl MacroArgs {
    fn parse(args: Punctuated<Meta, Comma>) -> Result<Self, syn::Error> {
        let mut macro_args = Self::default();

        for meta in args {
            match meta {
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("duration") {
                        if let Expr::Lit(ExprLit { lit: Lit::Str(lit_str), .. })= &nv.value {
                            macro_args.duration = parse_duration(&lit_str.value())?;
                        }
                        else {
                            return Err(syn::Error::new_spanned(&nv.value, "Expected duration as a string, e.g., duration = \"120s\""));
                        }
                    }
                    else {
                        return Err(syn::Error::new_spanned(nv.path, "Unknown argument. Did you mean 'duration'?"));
                    }
                }
                _ => {
                    return Err(syn::Error::new_spanned(meta, "Unsupported attribute argument. Use key-value pairs, e.g., duration = \"120s\""));
                }
            }
        }

        Ok(macro_args)
    }
}

fn parse_duration(s: &str) -> Result<proc_macro2::TokenStream, syn::Error> {
    if let Some(ms) = s.strip_suffix("ms") {
        if let Ok(ms) = ms.parse::<u64>() {
            return Ok(quote! { ::std::time::Duration::from_millis(#ms) });
        }
    }
    if let Some(m) = s.strip_suffix("m").or_else(|| s.strip_suffix("min")).or_else(|| s.strip_suffix("mins")) {
        if let Ok(mins) = m.parse::<u64>() {
            let secs = mins * 60;
            return Ok(quote! { ::std::time::Duration::from_secs(#secs) });
        }
    }
    if let Some(secs) = s.strip_suffix("s").or_else(|| s.strip_suffix("sec")).or_else(|| s.strip_suffix("secs")) {
        if let Ok(secs) = secs.parse::<u64>() {
            return Ok(quote! { ::std::time::Duration::from_secs(#secs) });
        }
    }
    Err(syn::Error::new_spanned(s, "Failed to parse duration. Use 'ms' (for milliseconds), 'm', 'min', 'mins' (for minutes), 's', 'sec', 'secs' (for seconds)"))
}

#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let args = parse_macro_input!(attr with Punctuated<Meta, Token![,]>::parse_terminated);

    let macro_args = match MacroArgs::parse(args) {
        Ok(args) => args,
        Err(e) => return e.to_compile_error().into()
    };

    let attrs = input.attrs;
    let vis = input.vis;
    let sig = input.sig;
    let body = input.block;
    let fn_name = &sig.ident;
    let duration = macro_args.duration;

    if sig.asyncness.is_none() {
        return syn::Error::new_spanned(sig.fn_token, "#[fracture::test] only supports async functions").to_compile_error().into();
    }

    let expanded = quote! {
        #[::core::prelude::v1::test]
        #(#attrs)*
        #vis fn #fn_name() {
            ::fracture::chaos::init_from_env();

            let mut sim = ::turmoil::Builder::new()
                .simulation_duration(#duration)
                .build();

            sim.client("test_client", async move {
                ::fracture::chaos::trace::clear_trace();
                ::fracture::chaos::invariants::reset();

                let checker_handle = ::fracture::task::spawn(async {
                    loop {
                        if !::fracture::chaos::invariants::check_all() {
                            break;
                        }
                        ::fracture::time::sleep::(::std::time::Duration::from_millis(100)).await;
                    }
                });

                let test_fut = async #body;
                let test_result = test_fut.await;

                checker_handle.abort();

                if ::fracture::chaos::invariants::has_violations() {
                    let violations = ::fracture::chaos::invariants::get_violations();
                    let trace = ::fracture::chaos::trace::get_trace();

                    let trace_log = trace.iter()
                        .map(|entry| format!("  - {:?} at {:?}", entry.event, entry.timestamp))
                        .collect::<Vec<String>>()
                        .join("\n");

                    panic!("\n\n❌ Invariant Violation(s) Detected: {:?}\n\nChaos Trace:\n{}\n\n", violations, trace_log)
                }

                test_result
            });

            sim.run().expect("Simulation failed");
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn main(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);

    let args = parse_macro_input!(attr with Punctuated<Meta, Token![,]>::parse_terminated);

    let macro_args = match MacroArgs::parse(args) {
        Ok(args) => args,
        Err(e) => return e.to_compile_error().into(),
    };

    let attrs = input.attrs;
    let vis = input.vis;
    let sig = input.sig;
    let body = input.block;
    let fn_name = &sig.ident;
    let duration = macro_args.duration;

    let ret = match sig.output {
        ReturnType::Default => quote! {},
        ReturnType::Type(_, ty) => quote! { -> #ty }
    };

    let expanded = quote! {
        #(#attrs)*
        #vis fn #fn_name() #ret {
            #[cfg(feature = "simulation")]
            {
                ::fracture::chaos::init_from_env();

                let mut sim = ::turmoil::Builder::new()
                    .simulation_duration(#duration)
                    .build();

                sim.client("main", async move {
                    #body
                });

                sim.run().expect("Simulation failed");
            }

            #[cfg(not(feature = "simulation"))]
            {
                ::tokio::runtime::Builder::new_multi_thread()
                    .enable_all()
                    .build()
                    .expect("Failed to build async runtime")
                    .block_on(async {
                        #body
                    })
            }
        }
    };

    TokenStream::from(expanded)
}