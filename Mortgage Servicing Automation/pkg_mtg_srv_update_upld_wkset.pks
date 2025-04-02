CREATE OR REPLACE PACKAGE pkg_mtg_srv_update_upld_wkset IS

  
  --
  -- Module Name : pkg_mtg_srv_load_wkset
  -- Purpose : The primary purpose of this package is to load the mortgage  update working set tables
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name : pkg_mtg_srv_load_wkset.pkb
  

  -------------------------------------------------------------------------
  --Module       p_mtg_srv_populate_mtg_srv_tran
  --Purpose      Temporary working set table which is created with all the loans
  --             which are not submitted for cancellation from the primary stage table, in addition
  --             it will have few more attributes for further processing.
  --Parameters: i_batch_id,i_run_part_id,i_tx_id,i_pcsg_dt
  -------------------------------------------------------------------------
  PROCEDURE p_mtg_srv_populate_mtg_srv_tran(i_batch_id    IN NUMBER,
                             i_run_part_id IN NUMBER,
                             i_tx_id       IN NUMBER,
                             i_pcsg_dt     IN DATE);

END pkg_mtg_srv_update_upld_wkset;
/
