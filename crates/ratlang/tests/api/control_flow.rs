use crate::helper::run_source;
use ratlang::Value;

#[test]
fn while_break_inside_if_propagates_out_of_expression() {
    let source = r#"
module tests.control_flow

fn main(args: List[str]) -> int
  var odd_acc = 0
  var probe = 0
  while probe < 15
    probe = probe + 1
    if probe % 2 == 0
      continue
    odd_acc = odd_acc + probe
    if probe >= 9
      break
  odd_acc
"#;

    let value = run_source("control_flow_break", source);
    match value {
        Value::Int(total) => assert_eq!(total, 25, "loop should stop once probe reaches nine"),
        other => panic!("expected integer result, got {other:?}"),
    }
}
