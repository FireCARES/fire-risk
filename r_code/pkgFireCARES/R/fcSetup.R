##  fcSetup.R
#' Condition a data set for use in model estimation
#'
#' Condition a data set for use in model estimation
#'
#' @param dta data.frame. The data set that needs to be condition for use.
#' @param seed Intger. A random seed used to ensure consistent results
#' for the partitioning of the data set into training and test sets.
#'
#' @return The conditioned data frame.
#'
#' @export
#' @details
#' This takes the 'low.risk.fires', 'med.risk.fires' and 'high.risk.fires'
#' data.frames as pulled from the database and makes the modification needed
#' to use them for analysis.
#'
#' It also does much of the pre-processing on the 'lr_mr_pred' and 'hr_pred'
#' data frames as well.
#'
#' This function carries out the following tasks:
#' \enumerate{
#'   \item Set any nulls in the outcome variables to zero.
#'   \item Turn any categorical predictors into factors.
#'   \item Take the log of the income variable.
#'   \item Ensure that there is an f_located column in the table.
#'   \item Define any filters that are needed (only for training tables).
#'   \item Define training and test sets for the training tables.
#' }
#'
fcSetup <- function(dta, seed=953016876)
{
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# low.risk.fires fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fdid, fd_id, fd_size
#  Info: dept_incidents, dept_fires, dept_lr, f_located
#  Outcomes: res_all, low_risk, res_1, res_2, res_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty, fuel_...
#
# med.risk fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fd_id, fd_size
#  Info: dept_incidents, dept_fires, f_located
#  Outcomes: med_risk, mr_1, mr_2, mr_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, apt_parcels, mr_parcels, inc_hh<<text>>,
#              svi, married, unemployed, nilf, smoke_st, smoke_cty
#
# high.risk fields
#  Index: year, parcel_id, geoid <<change to tr10_fid?>>, geoid_source, region, state, fd_id, fd_size
#  Preliminary: res_corelogic, res_other, bld_units, hr_floors, eff_yr, risk_class
#  Info: dept_incidents, dept_fires, f_located
#  Outcomes: fires, size_1, size_2, size_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty
#
# lr_mr_pred fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fd_id, fd_size
#  Info: f_located
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, apt_parcels, mr_parcels, inc_hh<<text>>,
#              fuel_..., svi, married, unemployed, nilf, smoke_st, smoke_cty
#
# hr_pred fields
#  Index: year, parcel_id, geoid <<change to tr10_fid?>>, geoid_source, region, state, fd_id, fd_size
#  Preliminary: res_corelogic, res_other, bld_units, hr_floors, eff_yr, risk_class
#  Info:
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty
#
# Identify what table this is:
    lr.tbl   <- 'res_all' %in% names(dta)
    ems.tbl  <- 'obesity' %in% names(dta)
    pred.tbl <- ! ('dept_incidents' %in% names(dta) | 'dept_calls' %in% names(dta))
    tr.id <- grep("tr10_fid|geoid$", names(dta), value=TRUE)
# Format the data as needed.
    v.factors <- intersect(c('region',  'state',    'fd_id', 'fd_size', 'fdid', 'fc_dept_id', 'cluster', 'res_corelogic',
                             'res_other', 'risk_class'), names(dta))
    v.zeros   <- intersect(c('res_all', 'low_risk', 'res_1', 'res_2',   'res_3',  'injuries', 'deaths',    'med_risk',
                             'mr_1',    'mr_2',     'mr_3',  'fires',   'size_1', 'size_2',   'size_3',    'hr_floors', 'ems' ), names(dta))
    v.na <- setdiff(names(dta), c(v.zeros, 'res_corelogic', 'eff_yr', 'geoid_source', 'bld_units'))
    for(i in v.zeros) dta[[i]][is.na(dta[[i]])] <- 0
    dta$region[dta$state == 'PR'] <- 'Puerto Rico'
    dta$region[dta$state %in% c('AK', 'HI')] <- 'West'
    dta$inc_hh <- log(as.numeric(dta$inc_hh))
    for(i in v.factors) dta[[i]] <- factor(dta[[i]])
    dta$region <- relevel(dta$region, 'West')
    if(! (ems.tbl |   'f_located' %in% names(dta))) dta$f_located <- 1
    if(   ems.tbl & ! 'c_located' %in% names(dta) ) dta$c_located <- 1
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Not all filters are needed if this is a medium or high risk data set.  !
#                                                                        !
# Filters:                                                               !
#  low_risk_fires:  base, lcl, small, no.fire, giants                    !
#  med_risk_fires:  base, lcl, small, no.fire                            !
#  high_risk_fires: base, lcl, small, no.fire                            !
#  lr_mr_pred:      <none>                                               !
#  hr_pred:         <none>                                               !
#                                                                        !
# The filters for the '_pred' tables are not needed because the          !
# prediction function (below) automatically screens out any rows with    !
# NULL predictors.                                                       !
#                                                                        !
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# create filters
    if(! pred.tbl){
        dta$no.fire <- dta$small <- dta$base <- dta$include <- TRUE

# base filter
        lvl <- grep("[3-9]", levels(dta$fd_size), value=TRUE)
        dta$base <- dta$base & dta$fd_size %in% lvl
        for(i in v.na) dta$base <- dta$base & ! is.na(dta[[i]])
        dta$base <- dta$base & dta$svi >= 0

        if(ems.tbl){
# EMS Filters
          dta$base <- dta$base & dta$c_located > 0.1
          dta$small <- dta$dept_calls     > 25 & ! is.na(dta$dept_calls)
        } else {
# Fire Filters
          dta$base <- dta$base & dta$f_located > 0
          dta$small <- dta$dept_incidents > 25 & ! is.na(dta$dept_incidents)
# no.fires filter
          fire.col <- grep("low_risk|med_risk|^fires", names(dta))
          dta$no.fire[dta[[fire.col]] == 0] <- FALSE

# define outliers (lcl)
          dept <- dta[, c(tr.id, 'year', 'fd_id', 'dept_incidents')]
          ddd <- unique(dta[,c('year', 'fd_id', 'dept_incidents')])
          ddd <- aggregate(ddd$dept_incidents, list(fd_id=ddd$fd_id),
                           function(x) c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
          ddd$m <- ddd$x[,1]
          ddd$sd <- ddd$x[,2]
          dept$m <- ddd$m[match(dept$fd_id, ddd$fd_id)]
          dept$sd <- ddd$sd[match(dept$fd_id, ddd$fd_id)]
          dept$lg <- ! (is.na(dept$dept_incidents) | dept$dept_incidents < dept$m - 2 * dept$sd)
          dept$lg[is.na(dept$lg)] <- FALSE
          dta$lcl <- dept$lg
          rm(dept, ddd)
        }

        if(lr.tbl) {
# giants filter
            u <- with(dta[dta$base,], list(pop      =dta[[tr.id]][  pop      > quantile(pop,      .999)],
                                            hse.units=dta[[tr.id]][hse_units > quantile(hse_units, .999)],
                                            males    =dta[[tr.id]][males     > quantile(males,     .999)],
                                            age_45_54=dta[[tr.id]][age_45_54 > quantile(age_45_54, .999)]))
            v <- NULL
            for(i in names(u)) v <- union(v, u[[i]])
            dta$giants <- ! dta[[tr.id]] %in% v
            rm(i, u, v)
        }
# Finish Filters
        filter <- intersect(c('base', 'small', 'lcl', 'giants'), names(dta))
        for(i in filter) dta$include <- dta$include & dta[[i]]
#
# partition data (do I use different seeds for each of the risk types?)
        set.seed(seed)
        tr10_fid <- unique(dta[[tr.id]])
        tr10_fid <- data.frame(tr10_fid=tr10_fid, v=floor(runif(length(tr10_fid)) * 3) + 1, set="",
                                stringsAsFactors=FALSE)
        tr10_fid$set <- c("training", "validation", "test")[tr10_fid$v]
        tr10_fid$set <- factor(tr10_fid$set)
        dta$set <- tr10_fid$set[match(dta[[tr.id]], tr10_fid$tr10_fid)]
    }
    dta
}
