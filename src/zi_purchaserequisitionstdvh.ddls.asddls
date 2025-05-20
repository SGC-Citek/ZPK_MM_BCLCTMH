@AbapCatalog.sqlViewName: 'ZSQL_PURREQ_VH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Requisition Header Value Help'
define view ZI_PurchaserequisitionStdVH
  as select from I_PurchaseRequisitionAPI01
{
  key PurchaseRequisition
}
