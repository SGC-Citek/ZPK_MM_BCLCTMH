@AbapCatalog.sqlViewName: 'ZSQL_PROGRP_VH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Product Group Value Help'
define view ZI_ProductGroupStdVH
  as select from I_ProductGroupText_2
{
  key ProductGroup,
      ProductGroupName
}
where
  Language = $session.system_language
