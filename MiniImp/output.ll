define i64 @func(i64 %input_val) {
entry:
  %a = alloca i64
  %b = alloca i64
  %i = alloca i64
  %n = alloca i64
  %res = alloca i64
  %tmp = alloca i64
  %x = alloca i64
  store i64 %input_val, ptr %n
  br label %bb_1

bb_1:
  store i64 10, ptr %x
  br label %bb_2
bb_2:
  br label %bb_3
bb_3:
  br label %bb_4
bb_4:
  br label %bb_5
bb_5:
  %tmp.1 = load i64, ptr %n
  %tmp.2 = icmp slt i64 %tmp.1, 1
  br i1 %tmp.2, label %bb_7, label %bb_8
bb_6:
  %tmp.3 = load i64, ptr %res
  ret i64 %tmp.3
bb_7:
  br label %bb_6
bb_8:
  store i64 0, ptr %a
  br label %bb_9
bb_9:
  store i64 1, ptr %b
  br label %bb_10
bb_10:
  store i64 1, ptr %i
  br label %bb_11
bb_11:
  %tmp.4 = load i64, ptr %i
  %tmp.5 = load i64, ptr %n
  %tmp.6 = add i64 %tmp.5, 1
  %tmp.7 = icmp slt i64 %tmp.4, %tmp.6
  br i1 %tmp.7, label %bb_13, label %bb_12
bb_12:
  br label %bb_6
bb_13:
  %tmp.8 = load i64, ptr %a
  %tmp.9 = load i64, ptr %b
  %tmp.10 = add i64 %tmp.8, %tmp.9
  store i64 %tmp.10, ptr %tmp
  br label %bb_14
bb_14:
  %tmp.11 = load i64, ptr %b
  store i64 %tmp.11, ptr %a
  br label %bb_15
bb_15:
  %tmp.12 = load i64, ptr %tmp
  store i64 %tmp.12, ptr %b
  br label %bb_16
bb_16:
  %tmp.13 = load i64, ptr %i
  %tmp.14 = add i64 %tmp.13, 1
  store i64 %tmp.14, ptr %i
  br label %bb_17
bb_17:
  %tmp.15 = load i64, ptr %x
  %tmp.16 = add i64 %tmp.15, 0
  store i64 %tmp.16, ptr %x
  br label %bb_18
bb_18:
  br label %bb_11
}
