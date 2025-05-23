% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/authoring.R
\name{get_summary}
\alias{get_summary}
\title{Summaries subtitle documents in Chats}
\usage{
get_summary(x, prompt = NULL, topic = NULL, model = NULL)
}
\arguments{
\item{x}{character.}

\item{prompt}{character. 요약을 명령하는 프롬프트.}

\item{topic}{character. 토픽.}

\item{model}{character. claude 모델 이름.}
}
\value{
character.
}
\description{
Chat 리스트에서 특정 subtitle의 내용들을 추출
}
\examples{
\dontrun{
prompt <- "한국 생명보험시장 전략가들이 말한 '{topic}'에서 공통적으로 이야기하는 내용을 한글로 요약해주세요."
get_summary(x, prompt = "3. 판매채널의 유형 및 전략", topic = "판매채널의 유형 및 전략")
}
}
