# nowdoc

Simplest here document.
Only two transformation.

* remove head newline if exist
* remove one space from '|', space, space, ..., ']'

```hs
hello = [nowdoc|
abc
def
ghi
|]
```

is

```hs
hello = "abc\ndef\nghi\n"
```

,

```hs
hello = [nowdoc|hello|]
```

is

```hs
hello = "hello"
```

and

```hs
hello = [nowdoc|
hello | ]
world |  ]
! |   ]
|]
```

is

```hs
hello = "hello |]\nworld| ]\n! |  ]"
```
