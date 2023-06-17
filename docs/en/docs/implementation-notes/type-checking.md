# Type checking

## Typing language

| variable | $x$ | data constructor | $K$ |
| type variable | $\alpha$ | type constructor | $T$ |

|              |          |                                                                                                                                                                                                                                                               |
| ------------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| terms        | $t$      | $x$ \| $t\ t$ \| $\lambda x\langle\colon\tau\rangle.\ t$ \| $t\ \overline{\tau}$ \| $\Lambda \overline{\alpha\colon\kappa}.\ t$ \|<br>$\texttt{let}\ \Gamma \ \texttt{in}\ t$ \| $\texttt{case}\ t\langle\colon \tau\rangle\ \{ \overline{p \rightarrow t}\}$ |
| patterns     | $p$      | $x$ \| \_ \| $K\ \overline{p}$                                                                                                                                                                                                                                |
| types        | $\tau$   | $\alpha$ \| $T$ \| $\tau\rightarrow\tau$ \| $\forall \overline{\alpha\langle\colon \kappa\rangle}.\ \tau$ \| $\tau\ \tau$                                                                                                                                     |
| kinds        | $\kappa$ | \* \| $\kappa\rightarrow\kappa$                                                                                                                                                                                                                               |
| definitions  | $\Delta$ | $\texttt{fun}\ x\,\texttt{=}\,\{\overline{\overline{p}\,\texttt{->}\,t}\}$ \| $\texttt{data}\ T\ \overline{\alpha\langle\colon\kappa\rangle}\,\texttt{=}\,\{\overline{K\colon\tau}\}$                                                                         |
| specificaion | $\Sigma$ | $\texttt{fun}\ x\colon\tau$ \| $\texttt{data}\ T\colon\kappa$                                                                                                                                                                                                 |
| context      | $\Gamma$ | $\varepsilon$ \| $\Delta,\ \Gamma$ \| $\Sigma,\ \Gamma$                                                                                                                                                                                                       |
