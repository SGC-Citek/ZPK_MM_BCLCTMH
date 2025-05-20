@AbapCatalog.sqlViewName: 'ZSQL_PURORD_VH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Order Value Help'
define view zI_PurchaseOrderStdVH
  as select from I_PurchaseOrderAPI01
{
  key PurchaseOrder
}
