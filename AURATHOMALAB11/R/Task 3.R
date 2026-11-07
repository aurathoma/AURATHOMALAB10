#' Differeensii Dhensiitii Naannoo HDI Herregu
#'
#' Faankishiniin kun dhimmamummaa dhensiitii (density difference) qabxiiwwan fiixee
#' mirgaa fi bitaa hammangaa daangaa (interval) keessatti garagarummaa jiru herrega.
#' Kunis Highest Density Interval (HDI) barbaaduuf gargaara.
#'
#' @param a1 Lakkofsa (numeric). Probaabiliitii eegee gadii (lower tail probability).
#' @param mu Lakkofsa (numeric). Giddu-galeessa (mean) raabsa noormaalii. Difeeltiin 10.
#' @param std Lakkofsa (numeric). Garaagalummaa staandaardii (standard deviation). Difeeltiin 5.
#' @param alpha Lakkofsa (numeric). Sadarkaa faayidaa (significance level). Difeeltiin 0.05.
#'
#' @return Lakkofsa garagarummaa dhensiitii qabxiiwwan lamaan gidduu jiru.
#' @export
#'
#' @examples
#' # Garaagalummaa dhensiitii a1 = 0.025 irratti barbaaduuf:
#' dendiff(a1 = 0.025, mu = 10, std = 5, alpha = 0.05)
#'
#' # uniroot fayyadamte hiika (root) HDI barbaaduuf:
#' uniroot(dendiff, interval = c(0, 0.05))
dendiff <- function(a1, mu = 10, std = 5, alpha = 0.05){
  a2 <- alpha - a1 # non ETI
  qa1 <- qnorm(a1, mean = mu, sd = std)
  qa2 <- qnorm(1-a2, mean = mu, sd = std)

  dnorm(qa1, mean = mu, sd = std) - dnorm(qa2, mean = mu, sd = std)
}
