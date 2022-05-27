// these functions ensure all labels in a program are unique. these can be parsed back into the original data they were mangled from, which isn't actually necessary, but is an intuitive proof that they'll all be unique
pub fn data_label(label: &str) -> String {
    format!("data_{label}")
}

pub fn local_label(function: &str, label: &str) -> String {
    format!("label_{}_{}_{function}_{label}", function.len(), label.len(),)
}

pub fn function_name(function: &str) -> String {
    format!("func_{function}")
}
