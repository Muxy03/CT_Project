; ModuleID = 'generated/output.ll'
source_filename = "generated/output.ll"

define i64 @func(i64 %input_val) {
entry:
  br label %bb_1

bb_1:                                             ; preds = %entry
  br label %bb_2

bb_2:                                             ; preds = %bb_1
  br label %bb_3

bb_3:                                             ; preds = %bb_6, %bb_2
  %res.0 = phi i64 [ 1, %bb_2 ], [ %tmp.6, %bb_6 ]
  %n.0 = phi i64 [ 5, %bb_2 ], [ %tmp.8, %bb_6 ]
  %tmp.2 = icmp slt i64 0, %n.0
  br i1 %tmp.2, label %bb_5, label %bb_4

bb_4:                                             ; preds = %bb_3
  ret i64 %res.0

bb_5:                                             ; preds = %bb_3
  %tmp.6 = mul i64 %res.0, %n.0
  br label %bb_6

bb_6:                                             ; preds = %bb_5
  %tmp.8 = sub i64 %n.0, 1
  br label %bb_3
}
