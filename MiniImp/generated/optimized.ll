; ModuleID = 'generated/output.ll'
source_filename = "generated/output.ll"

define i64 @func(i64 %input_val) {
entry:
  br label %bb_1

bb_1:                                             ; preds = %entry
  br label %bb_2

bb_2:                                             ; preds = %bb_1
  br i1 false, label %bb_4, label %bb_5

bb_3:                                             ; preds = %bb_9, %bb_4
  %res.0 = phi i64 [ 0, %bb_4 ], [ %res.1, %bb_9 ]
  ret i64 %res.0

bb_4:                                             ; preds = %bb_2
  br label %bb_3

bb_5:                                             ; preds = %bb_2
  br label %bb_6

bb_6:                                             ; preds = %bb_5
  br label %bb_7

bb_7:                                             ; preds = %bb_6
  br label %bb_8

bb_8:                                             ; preds = %bb_14, %bb_7
  %res.1 = phi i64 [ undef, %bb_7 ], [ %tmp.6, %bb_14 ]
  %i.0 = phi i64 [ 1, %bb_7 ], [ %tmp.10, %bb_14 ]
  %b.0 = phi i64 [ 1, %bb_7 ], [ %tmp.6, %bb_14 ]
  %a.0 = phi i64 [ 0, %bb_7 ], [ %b.0, %bb_14 ]
  %tmp.3 = icmp slt i64 %i.0, 7
  br i1 %tmp.3, label %bb_10, label %bb_9

bb_9:                                             ; preds = %bb_8
  br label %bb_3

bb_10:                                            ; preds = %bb_8
  %tmp.6 = add i64 %a.0, %b.0
  br label %bb_11

bb_11:                                            ; preds = %bb_10
  br label %bb_12

bb_12:                                            ; preds = %bb_11
  br label %bb_13

bb_13:                                            ; preds = %bb_12
  %tmp.10 = add i64 %i.0, 1
  br label %bb_14

bb_14:                                            ; preds = %bb_13
  br label %bb_8
}
