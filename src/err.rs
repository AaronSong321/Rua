#[derive(Debug, Eq, PartialEq)]
pub enum CalculatorError {
    InputTooBig,
    InputTooSmall,
}