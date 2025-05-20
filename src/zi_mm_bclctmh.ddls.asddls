@EndUserText.label: 'Báo cáo luồng chứng từ mua hàng'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_MM_BCLCTMH'
@UI: {
    headerInfo: {
        typeName: 'Báo cáo luồng chứng từ mua hàng',
        typeNamePlural: 'Báo cáo luồng chứng từ mua hàng',
        title: {
            type: #STANDARD,
            label: 'Báo cáo luồng chứng từ mua hàng'
        }
    }
}
define root custom entity ZI_MM_BCLCTMH
{
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'zI_PurchaseOrderStdVH',
      element                        :'PurchaseOrder' }
      } ]
      @UI                            : { selectionField : [ { position: 60 } ] }
      @EndUserText.label             : 'PO Number'
  key SoDH                           : ebeln;
  key ItemDH                         : ebelp;
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'ZI_PurchaserequisitionStdVH',
      element                        :'PurchaseRequisition' }
      } ]
      @UI                            : { selectionField : [ { position: 50 } ] }
      @EndUserText.label             : 'PR Number'
  key SoDNMH                         : abap.char(10);
  key ItemDNMH                       : abap.numc(5);
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'I_InboundDelivery',
      element                        :'InboundDelivery' }
      } ]
      @UI                            : { selectionField : [ { position: 90 } ] }
      @EndUserText.label             : 'Inbound Delivery'
  key InboundDelivery                : vbeln_vl;
  key InboundDeliveryItem            : zde_posnr;
  key HigherLvlItmOfBatSpltItm       : zde_posnr;
  key MaterialDocumentYear           : gjahr;
  key MaterialDocument               : mblnr;
  key MaterialDocumentItem           : mblpo;
  key FiscalYear                     : gjahr;
  key InvoiceDocument                : belnr_d;
  key InvoiceDocumentItem            : rblgp;
      PRType                         : abap.char(4);
      PRTypeDes                      : abap.char(20);
      BoPhanDNMH                     : abap.char(200);
      NguoiDNMH                      : abap.char(200);
      NguoiMuaHang                   : abap.char(200);
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'I_ProductStdVH',
      element                        :'Product' }
      } ]
      @UI                            : { selectionField : [ { position: 20 } ] }
      @EndUserText.label             : 'Material'
      MaHang                         : matnr;
      TenHang                        : abap.char(200);
      @Semantics.unitOfMeasure       : true
      DVT                            : meins;
      NgayDNMH                       : abap.dats;
      NgayNhanDNMH                   : abap.dats;
      @Semantics.quantity.unitOfMeasure   : 'DVT'
      SoLuongKH                      : abap.quan( 13, 3 );
      // Thực tế (Số lượng) (SUM SoLuongChuaNK)
      @Semantics.quantity.unitOfMeasure   : 'DVT'
      SoLuongTT                      : abap.quan( 13, 3 );
      // Còn lại (Số lượng)
      @Semantics.quantity.unitOfMeasure   : 'DVT'
      SoLuongTTConLai                : abap.quan( 13, 3 );
      @Semantics.currencyCode        : true
      CurrencyPO                     : waers;
      @Semantics.amount.currencyCode : 'CurrencyPO'
      DonGia                         : abap.curr( 23, 2 );
      //      VAT                       : mwskz;
      VAT                            : abap.char(3);
      VATCalc                        : abap.curr( 11, 2 );
      @Semantics.amount.currencyCode : 'CurrencyPO'
      NetAmount                      : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode : 'CurrencyPO'
      ThanhTienKH                    : abap.curr( 23, 2 );
      // Thực tế (Thành tiền)
      @Semantics.amount.currencyCode : 'CurrencyPO'
      ThanhTienTT                    : abap.curr( 23, 2 );
      // Còn lại (Thành tiền)
      @Semantics.amount.currencyCode : 'CurrencyPO'
      ThanhTienTTConLai              : abap.curr( 23, 2 );
      @UI                            : { selectionField : [ { position: 120 } ] }
      @EndUserText.label             : 'PR Delivery date'
      NgayNhanHangKH                 : abap.dats;
      // Thực tế (Ngày nhận hàng)
      @UI                            : { selectionField : [ { position: 130 } ] }
      @EndUserText.label             : 'Inbound Delivery date'
      NgayNhanHangTT                 : abap.dats;
      @Consumption.valueHelpDefinition    : [ {
      entity                         :{
      name                           :'I_Supplier_VH',
      element                        :'Supplier' }
      }]
      @UI                            : { selectionField          : [ { position: 40 } ] }
      @EndUserText.label             : 'Supplier'
      MaNCC                          : lifnr;
      TenNCC                         : abap.char(200);
      LyDoSuDung                     : abap.char(100);
      GhiChu                         : abap.char(100);
      @UI                            : { selectionField : [ { position: 80 } ] }
      @EndUserText.label             : 'Created PO on'
      @Consumption.filter            : { mandatory:true }
      NgayDH                         : abap.dats;
      // Ngày YC nhận
      @UI                            : { selectionField : [ { position: 110 } ] }
      @EndUserText.label             : 'PO Delivery date'
      NgayNhanHangYC                 : abap.dats;
      // Giao tại
      GiaoTai                        : abap.char( 255 );
      @Semantics.unitOfMeasure       : true
      DeliveryQuantityUnit           : meins;
      @Semantics.quantity.unitOfMeasure   : 'DeliveryQuantityUnit'
      SoLuongINB                     : abap.quan( 13, 3 );
      // Số lượng chưa tạo INB
      @Semantics.quantity.unitOfMeasure   : 'DeliveryQuantityUnit'
      SoLuongChuaTaoINB              : abap.quan( 13, 3 );
      @Semantics.unitOfMeasure       : true
      EntryUnit                      : meins;
      @Semantics.quantity.unitOfMeasure   : 'EntryUnit'
      SoLuongDaNK                    : abap.quan( 13, 3 );
      // Số lượng chưa nhập kho
      @Semantics.quantity.unitOfMeasure   : 'EntryUnit'
      SoLuongChuaNK                  : abap.quan( 13, 3 );
      UnitInv                        : bprme;
      @Semantics.quantity.unitOfMeasure   : 'UnitInv'
      SoLuongPark                    : bpmng;
      // SL chưa Park
      @Semantics.quantity.unitOfMeasure   : 'EntryUnit'
      SoLuongChuaPark                : abap.quan( 13, 3 );
      @Semantics.currencyCode        : true
      CurrencyInv                    : waers;
      @Semantics.amount.currencyCode : 'CurrencyInv'
      ThanhTienPark                  : wrbtr_cs;
      // Thành tiền chưa Park
      @Semantics.amount.currencyCode : 'CurrencyInv'
      TotalGoodsMvtAmtInCCCrcy       : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode : 'CurrencyInv'
      ThanhTienChuaPark              : abap.curr( 23, 2 );
      @Semantics.quantity.unitOfMeasure   : 'UnitInv'
      SoLuongInv                     : bpmng;
      // SL chưa Invoice
      @Semantics.quantity.unitOfMeasure   : 'EntryUnit'
      SoLuongChuaInv                 : abap.quan( 13, 3 );
      @Semantics.amount.currencyCode : 'CurrencyInv'
      ThanhTienInv                   : wrbtr_cs;
      // Thành tiền chưa Invoice
      @Semantics.amount.currencyCode : 'CurrencyInv'
      ThanhTienChuaInv               : abap.curr( 23, 2 );
      InvoiceStatus                  : rbstat;
      InvoiceStatusText              : abap.char(20);
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'I_PurchasingOrganization',
      element                        :'PurchasingOrganization' }
      } ]
      @UI                            : { selectionField : [ { position: 10 } ] }
      @EndUserText.label             : 'Purchasing Org'
      @Consumption.filter            : {
      mandatory                      : true
      }
      PurchasingOrganization         : abap.char(4);
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'ZI_ProductGroupStdVH',
      element                        :'MaterialGroup' }
      } ]
      @UI                            : { selectionField : [ { position: 30 } ] }
      @EndUserText.label             : 'Material Group'
      MaterialGroup                  : abap.char(9);
      @Consumption.valueHelpDefinition    : [ { entity: { name: 'I_ProductTypeText_2', element: 'ProductType' }, additionalBinding: [{
                                            element: 'Language', localConstant: 'E', usage: #FILTER }] } ]
      @UI                            : { selectionField : [ { position: 40 } ] }
      @EndUserText.label             : 'Material Type'
      ProductType                    : abap.char(4);
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'zi_user',
      element                        :'UserID' }
      } ]
      @UI                            : { selectionField : [ { position: 70 } ] }
      @EndUserText.label             : 'Created PO By'
      CreatedByUserPO                : usnam;
      @Consumption.valueHelpDefinition    : [ { entity :
      {
      name                           :'zi_user',
      element                        :'UserID' }
      } ]
      @UI                            : { selectionField : [ { position: 100 } ] }
      @EndUserText.label             : 'Created INB By'
      CreatedByUserINB               : usnam;
      @Consumption.valueHelpDefinition    : [{ entity: {
      name                           : 'ZFI_I_YES_NO_VH',
      element                        : 'value_low'
      } }]
      @EndUserText.label             : 'Tính VAT'
      @UI                            : { selectionField : [ { position: 140 } ] }
      @Consumption.filter            : { selectionType: #SINGLE }
      FlagCalVAT                     : zde_yes_no;
      NgayTaoPO                      : abap.dats(8);
      IsCompletelyDelivered          : abap_boolean;
      IsCompletelyDeliveredDes       : abap.char(20);
      IsReturnsItem                  : flag;
      NhomHang                       : abap.char(100);
      PurchaseOrderQuantityUnit      : abap.unit( 3 );

      CurGiaTriChiPhiPark            : waers;
      @Semantics.amount.currencyCode : 'CurGiaTriChiPhiPark'
      GiaTriChiPhiPark               : abap.dec( 23, 2 );
      CurGiaTriChiPhiInvoicePark     : waers;
      @Semantics.amount.currencyCode : 'CurGiaTriChiPhiInvoicePark'
      GiaTriChiPhiInvoice            : abap.dec( 23, 2 );
      HangTang                       : abap.char(1);
      Hop_dong                       : abap.char(255);
      @Semantics.quantity.unitOfMeasure   : 'PurchaseOrderQuantityUnit'
      NetPriceQuantity               : abap.quan( 13, 3 );
      PurchasingDocumentStatus       : abap.char(2);
      PurchasingDocumentStatusDes    : abap.char(30);
      PurchasingDocumentDeletionCode : abap.char(1);
}
