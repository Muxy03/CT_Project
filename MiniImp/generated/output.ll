define i64 @func(i64 %input_val) {
entry:
  %a = alloca i64
  %b = alloca i64
  %i = alloca i64
  %n = alloca i64
  %res = alloca i64
  %tmp = alloca i64
  store i64 %input_val, ptr %n
  br label %bb_1

bb_1:
  br label %bb_2
bb_2:
  br i1 false, label %bb_4, label %bb_5
bb_3:
  %tmp.1 = load i64, ptr %res
  ret i64 %tmp.1
bb_4:
  store i64 0, ptr %res
  br label %bb_3
bb_5:
  store i64 0, ptr %a
  br label %bb_6
bb_6:
  store i64 1, ptr %b
  br label %bb_7
bb_7:
  store i64 1, ptr %i
  br label %bb_8
bb_8:
  %tmp.2 = load i64, ptr %i
  %tmp.3 = icmp slt i64 %tmp.2, 7
  br i1 %tmp.3, label %bb_10, label %bb_9
bb_9:
  br label %bb_3
bb_10:
  %tmp.4 = load i64, ptr %a
  %tmp.5 = load i64, ptr %b
  %tmp.6 = add i64 %tmp.4, %tmp.5
  store i64 %tmp.6, ptr %tmp
  br label %bb_11
bb_11:
  %tmp.7 = load i64, ptr %b
  store i64 %tmp.7, ptr %a
  br label %bb_12
bb_12:
  %tmp.8 = load i64, ptr %tmp
  store i64 %tmp.8, ptr %b
  br label %bb_13
bb_13:
  %tmp.9 = load i64, ptr %i
  %tmp.10 = add i64 %tmp.9, 1
  store i64 %tmp.10, ptr %i
  br label %bb_14
bb_14:
  %tmp.11 = load i64, ptr %b
  store i64 %tmp.11, ptr %res
  br label %bb_8
}
