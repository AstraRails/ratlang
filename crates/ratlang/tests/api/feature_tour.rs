use std::path::PathBuf;

use crate::helper::run_file;
use ratlang::Value;

#[test]
fn feature_tour_example_runs_to_completion() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop(); // ratlang
    path.pop(); // crates
    path.push("examples");
    path.push("feature_tour.rat");

    let value = run_file(&path);
    match value {
        Value::Int(code) => assert_eq!(code, 0, "feature tour should exit successfully"),
        other => panic!("expected success exit code, got {other:?}"),
    }
}
