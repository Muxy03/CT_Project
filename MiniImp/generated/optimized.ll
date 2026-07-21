; ModuleID = 'generated/output.ll'
source_filename = "generated/output.ll"

define i64 @func(i64 %input_val) {
entry:
  br label %bb_1

bb_1:                                             ; preds = %entry
  br label %bb_2

bb_2:                                             ; preds = %bb_5, %bb_1
  %res.0 = phi i64 [ 1, %bb_1 ], [ %tmp.6, %bb_5 ]
  %n.0 = phi i64 [ %input_val, %bb_1 ], [ %tmp.8, %bb_5 ]
  %tmp.2 = icmp slt i64 0, %n.0
  br i1 %tmp.2, label %bb_4, label %bb_3

bb_3:                                             ; preds = %bb_2
  ret i64 %res.0

bb_4:                                             ; preds = %bb_2
  %tmp.6 = mul i64 %res.0, %n.0
  br label %bb_5

bb_5:                                             ; preds = %bb_4
  %tmp.8 = sub i64 %n.0, 1
  br label %bb_2
}
