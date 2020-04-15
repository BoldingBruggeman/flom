The internal pressure gradient (normalised by the reference density),
\begin{equation}\label{ipg-analytical}
\mbox{ipg}^x
=\int_z^{\eta} \partial_x b\,\mbox{d}\xi,\quad
\mbox{ipg}^y
=\int_z^{\eta} \partial_y b\,\mbox{d}\xi,\quad
\end{equation}
is discretised in a straight-forward way
in geopotential vertical coordinates (vertically varying \(\Delta z_k\)
and Cartesian horizontal
coordinates (constant \(\Delta x\) and \(\Delta y\)).

Note that \(\Delta z_{i,j,k_{\max}}\) for the surface layer
is variable in time and space (because of the variable sea surface elevation)
and differs for \(U\)points and \(V\)points.
Therefore, we need to define
\begin{equation}\label{def-dz}
\begin{array}{l}
\Delta z_{i,j,k}^U= \left\{
\begin{array}{ll}
\frac12 \left(\Delta z_{i+1,j,k_{\max}}+\Delta z_{i,j,k_{\max}} \right)
        & \mbox{ for } k=k_{\max}, \\ \\
        \Delta z & \mbox{ else,}
\end{array}
\right. \\ \\
\Delta z_{i,j,k}^V= \left\{
\begin{array}{ll}
\frac12 \left(\Delta z_{i,j+1,k_{\max}}+\Delta z_{i,j,k_{\max}} \right)
        & \mbox{ for } k=k_{\max}, \\ \\
        \Delta z & \mbox{ else.}
\end{array}
\right.
\end{array}
\end{equation}
Based on the discrete buoyancy field \(b_{i,j,k}\) the discretisation is
of the following form:
\begin{equation}\label{igp-discrete}
\begin{array}{l}
\mbox{ipg}^x_{i,j,k} =
\frac{1}{\Delta x} \left(\sum_{m=k+1}^{k_{\max}}
        \Delta z^U_{i,j,m} \left(b_{i+1,j,m}-b_{i,j,m} \right)
        +\frac{\Delta z^U_{i,j,k}}{2} \left(b_{i+1,j,k}-b_{i,j,k} \right)
\right) \\ \\
\mbox{ipg}^y_{i,j,k} =
\frac{1}{\Delta y} \left(\sum_{m=k+1}^{k_{\max}}
        \Delta z^V_{i,j,m} \left(b_{i,j+1,m}-b_{i,j,m} \right)
        +\frac{\Delta z^V_{i,j,k}}{2} \left(b_{i,j+1,k}-b_{i,j,k} \right)
\right).
\end{array}
