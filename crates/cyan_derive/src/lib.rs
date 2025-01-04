use proc_macro::TokenStream;
use proc_macro_error::{abort, emit_error, proc_macro_error};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Ident, Type};

#[proc_macro_error]
#[proc_macro_derive(Located)]
pub fn located_derive(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DeriveInput);

  let name = input.ident;
  let data = input.data;

  match data {
    | Data::Struct(data_struct) => handle_struct(name, data_struct),
    | Data::Enum(data_enum) => handle_enum(name, data_enum),
    | Data::Union(..) => abort!(name, "Located cannot be derived for unions"),
  }
}

fn handle_struct(name: Ident, data_struct: DataStruct) -> TokenStream {
  // Look for a field named "location" and of type `cyan_reporting::Location`.
  if let Some(location_field) = data_struct.fields.iter().find(|field| {
    field
      .ident
      .as_ref()
      .map_or(false, |ident| ident == "location")
  }) {
    let location_ident = location_field.ident.as_ref().unwrap();

    return TokenStream::from(quote! {
      impl cyan_reporting::Located for #name {
        fn location(&self) -> &cyan_reporting::Location {
          &self.#location_ident
        }
      }
    });
  }

  emit_error!(name, "Struct does not have a `location` field.");

  TokenStream::new()
}

fn handle_enum(name: Ident, data_enum: syn::DataEnum) -> TokenStream {
  let mut match_arms = Vec::new();
  let mut expected_arms_count = 0;

  for variant in data_enum.variants.iter() {
    let variant_name = &variant.ident;

    expected_arms_count += 1;

    match &variant.fields {
      // Named struct variants, e.g.
      //
      // enum Foo {
      //   Bar { value: i32, location: Location },
      // }
      | Fields::Named(fields_named) => {
        let location_field = fields_named.named.iter().find(|field| {
          field
            .ident
            .as_ref()
            .map_or(false, |ident| ident == "location")
        });

        // If we found a location field, add a match arm for it.
        if location_field.is_some() {
          match_arms.push(quote! {
            #name::#variant_name { location, .. } => location,
          });
        }
        // Otherwise, issue a warning and short-circuit with error.
        else {
          emit_error!(
            variant_name,
            "Struct variant '{}' does not have a 'location' field.",
            variant_name
          );

          break;
        }
      },
      // Unnamed (or tuple) variants, e.g.
      //
      // enum Foo {
      //   Bar(Baz, Cux),
      // }
      //
      // This case is hard limited to only one unnamed field, which is expected to be something
      // that implements `Located`.
      | Fields::Unnamed(fields_unnamed) => {
        if let Some(first_field) = fields_unnamed.unnamed.first() {
          if let Type::Path(type_path) = &first_field.ty {
            if type_path.path.segments.len() > 1 {
              emit_error!(
                variant_name,
                "Tuple variant's field has more than one segment, which is not supported."
              );

              continue;
            }

            match type_path.path.segments.first() {
              | Some(..) => {
                match_arms.push(quote! {
                  #name::#variant_name(located) => located.location(),
                });
              },
              | None => {
                emit_error!(
                  variant_name,
                  "Tuple variant's field does not have any segments."
                )
              },
            }
          }
        } else {
          emit_error!(variant_name, "Tuple variant has no fields.");
        }
      },
      // Unit variants, e.g.
      //
      // enum Foo {
      //   Bar,
      // }
      //
      // These can't have a location field, so we error out.
      | Fields::Unit => {
        if !match_arms.is_empty() {
          emit_error!(variant_name, "Unit variant has no fields.");
        }
      },
    }
  }

  if match_arms.is_empty() {
    emit_error!(name, "Enum has no valid variants.");

    return TokenStream::new();
  }

  if !match_arms.is_empty() && expected_arms_count != match_arms.len() {
    emit_error!(
      name,
      "Expected all variants to have either a `location` \
      field or contain value that has it, but only {} of them do(es).",
      match_arms.len()
    );

    return TokenStream::new();
  }

  // Emit the implementation with the match arms we collected.
  TokenStream::from(quote! {
    impl cyan_reporting::Located for #name {
      fn location(&self) -> &cyan_reporting::Location {
        match self {
          #(#match_arms)*
        }
      }
    }
  })
}
