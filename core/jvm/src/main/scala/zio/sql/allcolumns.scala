package zio.sql

import scala.annotation.implicitNotFound

trait AllColumnsModule { self: SelectModule with ExprModule =>

  @implicitNotFound(
    "SELECT * currently not supported on joined tables, derived tables and for table of size bigger than 22."
  )
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

    implicit def instance11[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Empty]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Empty]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Empty]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Empty]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Empty]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance12[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Empty]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Empty]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Empty]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Empty]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Empty]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance13[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Empty]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Empty]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Empty]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Empty]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Empty]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance14[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Empty]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Empty]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Empty]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Empty]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Empty]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance15[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Empty]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Empty]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Empty]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Empty]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Empty]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance16[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Empty]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Empty]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Empty]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Empty]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Empty]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance17[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Empty]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Empty]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Empty]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Empty]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Empty]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance18[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Empty]]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Empty]]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Empty]]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Empty]]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17 ++ columns._18

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Empty]]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance19[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17 ++ columns._18 ++ columns._19

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance20[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17 ++ columns._18 ++ columns._19 ++ columns._20

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance21[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17 ++ columns._18 ++ columns._19 ++ columns._20 ++ columns._21 

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

    implicit def instance22[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, TableType, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22]: ColumnsHelper.Aux[
      (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21], Expr[F22, TableType, A22]),
      TableType,
      F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22,
      SelectionSet.Cons[
        TableType,
        A1,
        SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Cons[TableType, A22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]
      ],
      (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22),
      A1,
      SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Cons[TableType, A22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]
    ] =
      new ColumnsHelper[(Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21], Expr[F22, TableType, A22]), TableType] {

        override type F      = F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22
        override type SelSet = SelectionSet.Cons[
          TableType,
          A1,
          SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10,SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Cons[TableType, A22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]
        ]

        override type ResultTypeRepr = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)

        override type ColumnHead    = A1
        override type SelectionTail =
           SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Cons[TableType, A22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]

        override def apply(columns:  (Expr[F1, TableType, A1], Expr[F2, TableType, A2], Expr[F3, TableType, A3], Expr[F4, TableType, A4], Expr[F5, TableType, A5], Expr[F6, TableType, A6], Expr[F7, TableType, A7], Expr[F8, TableType, A8], Expr[F9, TableType, A9], Expr[F10, TableType, A10], Expr[F11, TableType, A11], Expr[F12, TableType, A12], Expr[F13, TableType, A13], Expr[F14, TableType, A14], Expr[F15, TableType, A15], Expr[F16, TableType, A16], Expr[F17, TableType, A17], Expr[F18, TableType, A18], Expr[F19, TableType, A19], Expr[F20, TableType, A20], Expr[F21, TableType, A21], Expr[F22, TableType, A22])) = {
          val selection = columns._1 ++ columns._2 ++ columns._3 ++ columns._4 ++ columns._5 ++ columns._6 ++ columns._7 ++ columns._8 ++ columns._9 ++ columns._10 ++ columns._11 ++ columns._12 ++ columns._13 ++ columns._14 ++ columns._15 ++ columns._16 ++ columns._17 ++ columns._18 ++ columns._19 ++ columns._20 ++ columns._21 ++ columns._22 

          SelectBuilder[
            F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22,
            TableType,
            SelectionSet.Cons[
              TableType,
              A1,
              SelectionSet.Cons[TableType, A2, SelectionSet.Cons[TableType, A3, SelectionSet.Cons[TableType, A4, SelectionSet.Cons[TableType, A5, SelectionSet.Cons[TableType, A6, SelectionSet.Cons[TableType, A7, SelectionSet.Cons[TableType, A8, SelectionSet.Cons[TableType, A9, SelectionSet.Cons[TableType, A10, SelectionSet.Cons[TableType, A11, SelectionSet.Cons[TableType, A12, SelectionSet.Cons[TableType, A13, SelectionSet.Cons[TableType, A14, SelectionSet.Cons[TableType, A15, SelectionSet.Cons[TableType, A16, SelectionSet.Cons[TableType, A17, SelectionSet.Cons[TableType, A18, SelectionSet.Cons[TableType, A19, SelectionSet.Cons[TableType, A20, SelectionSet.Cons[TableType, A21, SelectionSet.Cons[TableType, A22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]
            ]
          ](selection)
        }
      } 

  }
   // format: on

}
