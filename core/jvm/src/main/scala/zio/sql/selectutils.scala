package zio.sql

// format: off
trait SelectUtilsModule { self: TableModule with ExprModule with InsertModule with SelectModule =>

  sealed case class InsertIntoBuilder[Source, AllColumnIdentities](
    table: Table.Source.Aux_[Source, AllColumnIdentities]
  ) {

      def apply[F, B <: SelectionSet[Source]](sources: Selection[F, Source, B]) =
        InsertBuilder[F, Source, AllColumnIdentities, B, sources.ColsRepr](table, sources)

      def apply[F1, B1](expr1: Expr[F1, Source, B1]) = {
        type B = SelectionSet.Cons[Source, B1, SelectionSet.Empty]

        val selection: Selection[F1, Source, B] = expr1

        InsertBuilder[F1, Source, AllColumnIdentities, B, selection.ColsRepr](table, selection)
      }
  
      def apply[F1, F2, B1, B2](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2]) = {
        val selection = expr1 ++ expr2

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]]

        InsertBuilder[Features.Union[F1, F2], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, B1, B2, B3](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3]) = {
        val selection = expr1 ++ expr2 ++ expr3

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]]

        InsertBuilder[Features.Union[Features.Union[F1, F2], F3], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, B1, B2, B3, B4](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, B1, B2, B3, B4, B5](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, B1, B2, B3, B4, B5, B6](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, B1, B2, B3, B4, B5, B6, B7](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, B1, B2, B3, B4, B5, B6, B7, B8](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, B1, B2, B3, B4, B5, B6, B7, B8, B9](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
      
      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }

      def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21], expr22: Expr[F22, Source, B22]) = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22

        type B = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Cons[Source, B22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]]

        InsertBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21], F22], Source, AllColumnIdentities, B, selection.ColsRepr](
           table,
           selection
        )
      }
  }

  sealed case class SelectByCommaBuilder() {
    def apply[F1, Source, B1](expr1: Expr[F1, Source, B1])(implicit i: Features.IsPartiallyAggregated[F1]) = {
      Selector[F1, Source, SelectionSet.Cons[Source, B1, SelectionSet.Empty], i.Unaggregated](expr1)
    }

    def apply[F1, F2, Source, B1, B2](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[F1, F2]]
    ): Selector[Features.Union[F1, F2], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]], i.Unaggregated] = {
        val selection = expr1 ++ expr2
        Selector[Features.Union[F1, F2], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, Source, B1, B2, B3](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[F1, F2], F3]]
    ): Selector[Features.Union[Features.Union[F1, F2], F3], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3
        Selector[Features.Union[Features.Union[F1, F2], F3], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, Source, B1, B2, B3, B4](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4]]
    ): Selector[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4
        Selector[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, Source, B1, B2, B3, B4, B5](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, Source, B1, B2, B3, B4, B5, B6](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, Source, B1, B2, B3, B4, B5, B6, B7](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, Source, B1, B2, B3, B4, B5, B6, B7, B8](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18]]
    ): Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]], i.Unaggregated] = {
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18
        Selector[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19]]
    ) = {
        type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19]

        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19
        Selector[F, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20]]
    ) = {
        type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20]
        
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20
        Selector[F, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21]]
    ) = {
        type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21]
        
        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21
        Selector[F, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21], expr22: Expr[F22, Source, B22])(implicit
        i: Features.IsPartiallyAggregated[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21], F22]]
    ) = {
        type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21], F22]

        val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22
        Selector[F, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Cons[Source, B22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]], i.Unaggregated](selection)
    }
  }

  final class SubselectPartiallyApplied[ParentTable] {
    def apply[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]): SubselectBuilder[F, A, B, ParentTable] =
      SubselectBuilder(selection)
  
    def apply[F1, F2, Source, B1, B2](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2]): SubselectBuilder[Features.Union[F1, F2], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]], ParentTable] = {
      val selection = expr1 ++ expr2
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, Source, B1, B2, B3](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3]): SubselectBuilder[Features.Union[Features.Union[F1, F2], F3], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, Source, B1, B2, B3, B4](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4]): SubselectBuilder[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, Source, B1, B2, B3, B4, B5](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, Source, B1, B2, B3, B4, B5, B6](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, Source, B1, B2, B3, B4, B5, B6, B7](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, Source, B1, B2, B3, B4, B5, B6, B7, B8](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17]): SubselectBuilder[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]], ParentTable] = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17
      SubselectBuilder(selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18]) = {
      type S = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]]
      type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18]
      
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18
      
      SubselectBuilder[F, Source, S, ParentTable](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19]) = {
      type S = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]
      type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19]
      
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19
      SubselectBuilder[F, Source, S, ParentTable](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20]) = {
      type S = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]
      type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20]

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20
      SubselectBuilder[F, Source, S, ParentTable](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21]) = {
      type S = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]
      type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21]
      
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21
      SubselectBuilder[F, Source, S, ParentTable](selection)
    }

    def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21], expr22: Expr[F22, Source, B22]) = {
      type S = SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Cons[Source, B22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]]
      type F = Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[F1, F2], F3], F4], F5], F6], F7], F8], F9], F10], F11], F12], F13], F14], F15], F16], F17], F18], F19], F20], F21], F22]
      
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22
      SubselectBuilder[F, Source, S, ParentTable](selection)
    }  
  }
}
// format: on
