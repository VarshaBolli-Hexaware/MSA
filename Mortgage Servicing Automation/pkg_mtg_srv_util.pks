CREATE OR REPLACE PACKAGE pkg_mtg_srv_util AS
  
  -- Module Name    : pkg_mtg_srv_util
  -- Purpose        :
  -- stream      : Mortgage
  -- substream      : Servicing
  -- File Name       : pkg_mtg_srv_util.pks
  --
  
  --
  -- Last Update & Creation Date
  gv_upd_dt DATE := SYSDATE;

  -------------------------------------------------------------------------
  --Module      p_raise_fallout
  --Purpose     Log the fallouts and raise exception
  --Parameters
  -------------------------------------------------------------------------
  PROCEDURE p_raise_fallout(io_ctx        IN OUT pkg_log_util.gt_log_first,
                            i_fallout_tbl IN VARCHAR2,
                            i_fallout_col IN VARCHAR2,
                            i_base_tbl    IN VARCHAR2,
                            i_expd_cnt    IN NUMBER DEFAULT 0,
                            i_actl_cnt    IN NUMBER DEFAULT 0);

END pkg_mtg_srv_util;
/