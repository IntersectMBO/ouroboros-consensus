\documentclass[11pt,a4paper,dvipsnames,twosided]{article}
\usepackage[deliverable]{IOHKCoverPage}

\DeliverableTitle{A Formal Specification of the Cardano Consensus}{Formal Cardano Consensus Spec.}
\DeliverableResponsible{Formal Methods Team}
\EditorName{Javier D\'{i}az, \IOHK}
\Authors{Javier D\'{i}az \quad \texttt{<javier.diaz@iohk.io>}}
\LeaderName{James Chapman, \IOHK}
\InstitutionAddress{\IOHK}
\Version{1.0}
\DisseminationPU

\usepackage[margin=2.5cm]{geometry}
\usepackage{lscape}
\usepackage{iohk}
\usepackage{microtype}
\usepackage{mathpazo} % nice fonts
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{latexsym}
\usepackage{mathtools}
\usepackage{stmaryrd}
\usepackage{extarrows}
\usepackage{slashed}
\usepackage[unicode=true,pdfa,colorlinks=true]{hyperref}
\usepackage{xcolor}
\usepackage[capitalise,noabbrev,nameinlink]{cleveref}
\usepackage{float}
\floatstyle{boxed}
\restylefloat{figure}
\usepackage{tikz}
\usepackage{booktabs}
\usepackage{enumerate}
\usepackage{listings}
%%
%% Package `semantic` can be used for writing inference rules.
%%
\usepackage{semantic}
%% Setup for the semantic package
\setpremisesspace{20pt}

\usepackage{tocloft}
\addtolength{\cftsubsecnumwidth}{5pt}

\theoremstyle{definition}
\newtheorem{definition}{Definition}[section]
\newtheorem{lemma}[definition]{Lemma}
\newtheorem{theorem}[definition]{Theorem}
\newtheorem{property}{Property}[section]

\newcommand{\leteq}{\ensuremath{\mathrel{\mathop:}=}}

%
% Agda stuff
%
\usepackage{iohk}
\usepackage{agda-latex-macros}
\usepackage{hyperref}
\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    urlcolor=blue
}
\usepackage[links]{agda}
\setlength{\mathindent}{10pt}
\usepackage{fontspec}
\newcommand\agdaFont{StrippedJuliaMono}
\newcommand\agdaFontOptions{
Path=fonts/,
Extension=.ttf,
UprightFont=*-Regular,
BoldFont=*-Bold,
ItalicFont=*-RegularItalic,
BoldItalicFont=*-BoldItalic,
Scale=0.80
}
\newfontfamily{\AgdaSerifFont}{\agdaFont}[\agdaFontOptions]
\newfontfamily{\AgdaSansSerifFont}{\agdaFont}[\agdaFontOptions]
\newfontfamily{\AgdaTypewriterFont}{\agdaFont}[\agdaFontOptions]
\renewcommand{\AgdaFontStyle}[1]{{\AgdaSansSerifFont{}#1}}
\renewcommand{\AgdaKeywordFontStyle}[1]{{\AgdaSansSerifFont{}#1}}
\renewcommand{\AgdaStringFontStyle}[1]{{\AgdaTypewriterFont{}#1}}
\renewcommand{\AgdaCommentFontStyle}[1]{{\AgdaTypewriterFont{}#1}}
\renewcommand{\AgdaBoundFontStyle}[1]{{\emph{\AgdaTypewriterFont{}#1}}}

\begin{document}

\input{frontmatter.tex}
\cleardoublepage%

\begin{code}[hide]
{-# OPTIONS --safe #-}

module Spec.PDF where

\end{code}

\input{Ledger/Crypto}

\include{sts-overview}
\include{ledger-interface}
\include{blockchain}
\include{properties}
\include{leader-value}

\clearpage

\bibliographystyle{plain}
\bibliography{references}

\end{document}

\begin{code}[hide]

import Spec.TickNonce.Properties
import Spec.UpdateNonce.Properties
import Spec.OperationalCertificate.Properties
import Spec.Protocol.Properties
import Spec.TickForecast.Properties
import Spec.ChainHead.Properties

\end{code}
