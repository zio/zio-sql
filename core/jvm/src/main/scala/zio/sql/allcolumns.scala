package zio.sql

import scala.annotation.implicitNotFound

trait AllColumnsModule { self: SelectModule with ExprModule =>

  @implicitNotFound("SELECT * not supported for table of this size")
  sealed trait ColumnsHelper[ColumnsOut, TableType] {
    type F
    type SelSet <: SelectionSet[TableType]
    type ResultTypeRepr
    type ColumnHead
    type SelectionTail <: SelectionSet[TableType]

    def apply(columns: ColumnsOut): SelectBuilder[F, TableType, SelSet]
  }

  // format: off
  object ColumnsHelper {

    type Aux[ColumnsOut0, TableType0, F0, SelectionSet0, ResultTypeRepr0, ColumnsHead0, SelectionTail0] =
      ColumnsHelper[ColumnsOut0, TableType0] {
        type F = F0

        type SelSet = SelectionSet0

        type ResultTypeRepr = ResultTypeRepr0
        type ColumnHead     = ColumnsHead0
        type SelectionTail  = SelectionTail0
      }

    implicit def instance1[F1, TableType, A1]: ColumnsHelper.Aux[Expr[
      F1,
      TableType,
      A1
    ], TableType, F1, SelectionSet.Cons[TableType, A1, SelectionSet.Empty], A1, A1, SelectionSet.Empty] =
      new ColumnsHelper[Expr[F1, TableType, A1], TableType] {

        override type F      = F1
        override type SelSet = SelectionSet.Cons[TableType, A1, SelectionSet.Empty]

        override type ResultTypeRepr = A1

        override type ColumnHead    = A1
        override type SelectionTail = SelectionSet.Empty

        override def apply(columns: Expr[F1, TableType, A1]) =
          SelectBuilder[F1, TableType, SelectionSet.Cons[TableType, A1, SelectionSet.Empty]](columns)
      }

    implicit def instance2[F1, F2, TableType, A1, A2]
      : ColumnsHelper.Aux[(Expr[F1, TableType, A1], Expr[F2, TableType, A2]), TableType, F1 with F2, SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Empty]
      ], (A1, A2), A1, SelectionSet.Cons[TableType, A2, SelectionSet.Empty]] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2]), TableType] {

        override type F      = F1 with F2
        override type SelSet = SelectionSet.Cons[TableType, A1, SelectionSet.Cons[TableType, A2, SelectionSet.Empty]]

        override type ResultTypeRepr = (A1, A2)

        override type ColumnHead    = A1
        override type SelectionTail = SelectionSet.Cons[TableType, A2, SelectionSet.Empty]

        override def apply(columns: (Expr[F1, TableType, A1], Expr[F2, TableType, A2])) = {
          val selection = columns._1 ++ columns._2

          SelectBuilder[
            F1 with F2,
            TableType,
            SelectionSet.Cons[TableType, A1, SelectionSet.Cons[TableType, A2, SelectionSet.Empty]]
          ](selection)
        }
      }
    
    implicit def instance3[F1, F2, F3, TableType, A1, A2, A3]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3]),
      TableType,
      F1 with F2 with F3,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Empty]]
      ],
      (A1, A2, A3),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Empty]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3]), TableType] {

        override type F      = F1 with F2 with F3
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Empty]]
        ]

        override type ResultTypeRepr = (A1, A2, A3)

        override type ColumnHead    = A1
        override type SelectionTail =
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Empty]]

        override def apply(columns: (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3

          SelectBuilder[
            F1 with F2 with F3,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Empty]]
            ]
          ](selection)
        }
      }
    

    implicit def instance4[F1, F2, F3, F4, TableType, A1, A2, A3, A4]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4]),
      TableType,
      F1 with F2 with F3 with F4,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Empty]]]
      ],
      (A1, A2, A3, A4),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Empty]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4]), TableType] {

        override type F      = F1 with F2 with F3 with F4
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Empty]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Empty]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4

          SelectBuilder[
            F1 with F2 with F3 with F4,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Empty]]]
            ]
          ](selection)
        }
      }

    implicit def instance5[F1, F2, F3, F4, F5, TableType, A1, A2, A3, A4, A5]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5]),
      TableType,
      F1 with F2 with F3 with F4 with F5,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Empty]]]]
      ],
      (A1, A2, A3, A4, A5),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Empty]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Empty]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Empty]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Empty]]]]
            ]
          ](selection)
        }
      }

    implicit def instance6[F1, F2, F3, F4, F5, F6, TableType, A1, A2, A3, A4, A5, A6]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Empty]]]]]
      ],
      (A1, A2, A3, A4, A5, A6),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Empty]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Empty]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Empty]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Empty]]]]]
            ]
          ](selection)
        }
      }

    implicit def instance7[F1, F2, F3, F4, F5, F6, F7, TableType, A1, A2, A3, A4, A5, A6, A7]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Empty]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Empty]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Empty]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Empty]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Empty]]]]]]
            ]
          ](selection)
        }
      }

    implicit def instance8[F1, F2, F3, F4, F5, F6, F7, F8, TableType, A1, A2, A3, A4, A5, A6, A7, A8]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Empty]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Empty]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Empty]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Empty]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Empty]]]]]]]
            ]
          ](selection)
        }
      }

    implicit def instance9[F1, F2, F3, F4, F5, F6, F7, F8, F9, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Empty]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Empty]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Empty]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Empty]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Empty]]]]]]]]
            ]
          ](selection)
        }
      }


    implicit def instance10[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Empty]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Empty]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Empty]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Empty]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Empty]]]]]]]]]
            ]
          ](selection)
        }
      } 

   // TODO add more arities or rewrite to macro of possible 
  }
   // format: on

}
