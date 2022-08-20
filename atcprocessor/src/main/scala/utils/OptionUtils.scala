package utils

import cats.Monad
import cats.mtl.*

extension [A](o: Option[A])
  def or[F[_]: Monad, Error](error: Error)(using err: Raise[F, Error]): F[A] =
    o.fold(err.raise(error))(Monad[F].pure)
