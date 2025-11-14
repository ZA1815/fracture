use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn, ReturnType};

#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let attrs = input.attrs;
    let vis = input.vis;
    let sig = input.sig;
    let body = input.block;
    let fn_name = &sig.ident;

    let attr_tokens = proc_macro2::TokenStream::from(attr);
    let config = if attr_tokens.is_empty() {
        quote! {}
    }
    else {
        quote! { #attr_tokens }
    };

    let is_async = sig.asyncness.is_some();

    let expanded = if is_async {
        quote! {
            #[::core::prelude::v1::test]
            #(#attrs)*
            #vis fn #fn_name() {
                ::fracture::chaos::init_from_env();

                let mut sim = ::turmoil::Builder::new()
                    .simulation_duration(::std::time::Duration::from_secs(60))
                    .build();

                sim.client("test_client", async move {
                    let result = async #body.await;

                    if ::fracture::chaos::invariants::has_violations() {
                        let violations = ::fracture::chaos::invariants::get_violations();
                        panic!("Invariant violations detected: {:?}", violations);
                    }

                    result
                });

                sim.run().expect("Simulation failed");
            }
        }
    }
    else {
        quote! {
            #[::core::prelude::v1::test]
            #(#attrs)*
            #vis fn #fn_name() {
                ::fracture::chaos::init_from_env();

                #body

                if ::fracture::chaos::invariants::has_violations() {
                    let violations = ::fracture::chaos::invariants::get_violations();
                    panic!("Invariant violations detected: {:?}", violations);
                }
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn main(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let attrs = input.attrs;
    let vis = input.vis;
    let sig = input.sig;
    let body = input.block;
    let fn_name = &sig.ident;

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
                    .simulation_duration(::std::time::Duration::from_secs(3600))
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