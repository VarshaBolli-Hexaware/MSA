CREATE OR REPLACE PACKAGE pkg_mtg_srv_load_bhvr_attr AS
  
  --------------------------------------------------------------------------------------------------
  --
  -- Module Name    : pkg_mtg_srv_load_bhvr_attr
  -- Purpose        : Reads data from working set tables and populates table with all
  --                  required behavior attributes.
  -- stream      : Mortgage
  -- substream      : Servicing
  -- File Name       : pkg_mtg_srv_load_bhvr_attr.pks
  ------------------------------------------------------------------------------------------------

  -------------------------------------------------------------------------------------------
  --Module      p_mtg_srv_populate_loan
  --Purpose     Create new loan behavior attributes, populate  table  with all
  --            required attribute names and their values (Refer Mapping Table) and populate
  --             table with new bank_ast_id and their corresponding new and old
  --            attribute values with action code as UPDATE.
  --Parameter   i_pcsg_dt
  -------------------------------------------------------------------------------------------

  PROCEDURE p_mtg_srv_populate_loan(i_pcsg_dt IN DATE);

  ----------------------------------------------------------------------------
  -- Module    : f_get_mtg_srv_attr_nme
  --Purpose    : Get mtg_srv_attr_nme for the given tbl_nme, col_nme 
  --
  -------------------------------------------------------------------------
  FUNCTION f_get_mtg_srv_attr_nme (i_tbl_nme IN VARCHAR2, 
                               i_col_nme IN VARCHAR2,
                               i_mtg_srv_attr_tbl_typ_cd IN NUMBER DEFAULT NULL )
  RETURN VARCHAR2;

    FUNCTION f_add_cycles(i_starting_date   IN DATE,
                        i_num_periods     IN PLS_INTEGER,
                        i_pmt_cyl_id      IN pmt_cyl.pmt_cyl_id%TYPE,
                        i_pmt_cyl_frqy_cd IN pmt_cyl.pmt_cyl_frqy_cd%TYPE)
  RETURN DATE DETERMINISTIC;

    FUNCTION f_normalize_for_pc_chg(i_eff_dt           IN DATE,
                                  i_new_pmt_cyl_id   IN PLS_INTEGER,
                                  i_prcs_dt_clctn_id IN PLS_INTEGER)
  RETURN DATE DETERMINISTIC;

END pkg_mtg_srv_load_bhvr_attr;
/