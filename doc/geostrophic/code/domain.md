Sets up the calculation domain for the geostrophic model.
All domain and grid related quantities are set and calculated in 
this module like - dimensions, coordinates, metrics and related
meta-data.
For now the Arakawa A-grid is used.
Therefore, we need to define
\begin{equation}\label{def-dz}
\begin{array}{l}
\displaystyle
\Delta z_{i,j,k}^U= \left\{
\begin{array}{ll}
\frac12 \left(\Delta z_{i+1,j,k_{\max}}+\Delta z_{i,j,k_{\max}} \right)
        & \mbox{ for } k=k_{\max}, \\ \\
        \Delta z & \mbox{ else,}
\end{array}
\right. \\ \\
\displaystyle
\Delta z_{i,j,k}^V= \left\{
\begin{array}{ll}
\frac12 \left(\Delta z_{i,j+1,k_{\max}}+\Delta z_{i,j,k_{\max}} \right)
        & \mbox{ for } k=k_{\max}, \\ \\
        \Delta z & \mbox{ else.}
\end{array}
\right.
\end{array}
\end{equation}
