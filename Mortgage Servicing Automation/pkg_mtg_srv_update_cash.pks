CREATE OR REPLACE PACKAGE pkg_mtg_srv_update_cash AS
/*
 * Copyright (c) 2025 Company. All rights reserved.
 */
  -- Module Name    : pkg_mtg_srv_update_cash
  -- Purpose        : This Package is for cash transaction processing
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name       : pkg_mtg_srv_update_cash.pks


 ---This procedure will call staging procedure and invoke cash processing API----
 PROCEDURE p_mtg_srv_perform_cash(i_lpc_tran_pcsg_dt IN DATE);

END pkg_mtg_srv_update_cash;
/