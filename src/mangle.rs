// these functions ensure all labels in a program are unique. these can be parsed back into the original data they were mangled from, which isn't actually necessary, but is an intuitive proof that they'll all be unique

fn encode(label: &str) -> String {
    let mut result = String::with_capacity(label.len());
    for ch in label.chars() {
        match ch {
            '.' => result.push_str("_dot_"),
            '_' => result.push_str("_under_"),
            ch => result.push(ch),
        }
    }
    result
}

pub fn data_label(label: &str) -> String {
    format!("data__{}", encode(label))
}

pub fn local_label(function: &str, label: &str) -> String {
    format!(
        "label__{}__{}",
        encode(function.trim_start_matches('$')),
        encode(label)
    )
}

pub fn function_name(function: &str) -> String {
    format!("func__{}", encode(function.trim_start_matches('$')))
}
