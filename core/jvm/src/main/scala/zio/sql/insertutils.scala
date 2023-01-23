package zio.sql

// format: off
trait InsertUtilsModule {  self: ExprModule with TableModule with InsertModule with SelectModule =>

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
}
// format: on
