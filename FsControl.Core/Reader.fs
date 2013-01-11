namespace FsControl.Core.Types

open FsControl.Core.Abstractions
open FsControl.Core.Abstractions.Applicative

type Reader<'r,'a> = ('r->'a)

module Reader =
    let mapReader  f (m: Reader<'r, 'a>) = f << m :Reader<'r,_>
    let ask() = id
    let local f (m: Reader<'r, 'a>) = m << f :Reader<'r,_>
