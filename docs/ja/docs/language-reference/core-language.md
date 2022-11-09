# コア言語

## コア言語の構文

$$
\begin{align*}
t::\:   & & & \textsf{term:} \\
        & x &   & \textsf{variable} \\
        & t\:t & & \textsf{application} \\
        & \lambda x.\:t & & \textsf{abstraction} \\
        & t\:T & & \textsf{type application} \\
        & \Lambda X.\:t & & \textsf{type abstraction} \\
        & \texttt{let}\:x\:\texttt{=}\:t\:\texttt{in}\:t & & \textsf{let expression} \\
        & \texttt{fix}\:t & & \textsf{fix combinator} \\
        & t.x & & \textsf{projection} \\
        & \{x_i\:\texttt{=}\:t_i\}^{i \in 1..n} & & \textsf{record} \\
        & \texttt{<}x\:\{t_i\}^{i \in 1..n} : T \texttt{>} & & \textsf{tag value} \\
        & \texttt{case}\:t\:\texttt{of}\:\{k_i\:\rightarrow\:t_i\}^{i \in 1..n} & & \textsf{case expression} \\
        & \texttt{fold}\:T\:t  & & \textsf{fold} \\
        & \texttt{unfold}\:T\:t  & & \textsf{unfold} \\
        \\
T::\:   & & & \textsf{type:} \\
        & X &                                  & \textsf{type variable} \\
        & T \rightarrow T &                              & \textsf{type of functions} \\
        & \forall X:K.\:T &                    & \textsf{universal type} \\
        & T\:T &                                 & \textsf{operator application} \\
        & \lambda X:K.\:T &                            & \textsf{operator abstraction} \\
        & \mu X:K.\:T & & \textsf{recursive type} \\
        & \{x_i : T_i\}^{i \in 1..n} &                        & \textsf{type of record} \\
        & \{x_i: \{T_j\}^{j \in 1..M_i}\}^{i \in 1..n} &                      & \textsf{variant type} \\
        \\
K::\:   & & & \textsf{kind:} \\
        & * & & \textsf{kind of proper types} \\
        & K \rightarrow K & & \textsf{kind of operators} \\
        \\
\Gamma::\: & & & \textsf{context:} \\
          & \varnothing & & \textsf{empty context} \\
          & \Gamma,\:x : T & & \textsf{variable binding} \\
          & \Gamma,\:x : T = t & & \textsf{function binding} \\
          & \Gamma,\:X : K = T & & \textsf{type binding}
\end{align*}
$$

## モジュール

### モジュールの構文

$$
\begin{align*}
M::\:\Gamma\:\times\:\{t_i : T_i\}^{i \in 1..n}
\end{align*}
$$

### モジュールのセマンティクス

$$
\frac{\Gamma_0 \vdash M}{\Gamma_0,\:\Gamma \vdash \{t_i : T_i\}^{i \in 1..n}}{\:\text{E-Module}}
$$

## Commands

核言語において、プログラム全体は Commands というデータ型に変換される。
Commands はインポートされたモジュールと、トップレベルで定義された型コンストラクタ、データコンストラクタの記録、そして、main 関数からなる。main 関数は、Let 式で表され、トップレベルの関数が束縛に、ソースプログラムに現れる main 関数の項が Let 式の本体に当てられる。このように翻訳することにより、相互再帰関数が扱いやすくなる。

変数は抽象された識別子の相対位置として自然数で表される。これを de Bruijn インデックスという。
トップレベルで定義された型コンストラクタやデータコンストラクタは文脈（Context）に大域的に保持され、
プログラム中のどこでも参照することができる。
