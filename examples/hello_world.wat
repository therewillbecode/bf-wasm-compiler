(module
  (type $t0 (func (param i32 i32) (result i32)))
  (func $subtract (export "subtract") (type $t0) (param $lhs i32) (param $rhs i32) (result i32)
    get_local $lhs
    get_local $rhs
    i32.sub)
  
  (func $add3 (export "add3") type $t0)
    )
