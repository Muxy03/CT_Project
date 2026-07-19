define i64 @func(i64 %input_val) {
entry:
  %n = alloca i64
  %res = alloca i64
  store i64 %input_val, ptr %n
  br label %bb_1

bb_1:
  store i64 5, ptr %n
  br label %bb_2
bb_2:
  store i64 1, ptr %res
  br label %bb_3
bb_3:
  %tmp.1 = load i64, ptr %n
  %tmp.2 = icmp slt i64 0, %tmp.1
  br i1 %tmp.2, label %bb_5, label %bb_4
bb_4:
  %tmp.3 = load i64, ptr %res
  ret i64 %tmp.3
bb_5:
  %tmp.4 = load i64, ptr %res
  %tmp.5 = load i64, ptr %n
  %tmp.6 = mul i64 %tmp.4, %tmp.5
  store i64 %tmp.6, ptr %res
  br label %bb_6
bb_6:
  %tmp.7 = load i64, ptr %n
  %tmp.8 = sub i64 %tmp.7, 1
  store i64 %tmp.8, ptr %n
  br label %bb_3
}
