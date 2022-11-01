## 序文

Plato は定理証明を目的として設計された純関数型プログラミング言語である。構文は Haskell や Agda、Idris に影響を受けている。型付きラムダ計算を核言語に用い、再帰型、パラメトリック多相、高階型演算子などの強力な型システムを備えている。型署名及び、項レベルの型適用の明示が必須であり、核言語への翻訳には曖昧さが生じない。ブール値や自然数、リストを含め、リテラルや組み込み型を一切持たないという革新的な言語設計を採用しており、関数や代数的データ型の定義、任意の項を持つ変数の仮定のみの組み合わせて記述するのが特徴である。その他、ユーザー定義の演算子、カインド推論、モジュールシステムなど幅広い機能を有する。