CREATE OR REPLACE PACKAGE BODY pkg_mtg_srv_util AS

  -- Module Name    : pkg_mtg_srv_util
  -- Purpose        : Utility Package for the Reversals Service
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name       : pkg_mtg_srv_util.pkb
  --
  
  ----------------------------------------------------------------------------------
  --  Module      p_raise_fallout
  --  Purpose     Log the fallouts and raise application exception
  ----------------------------------------------------------------------------------

  PROCEDURE p_raise_fallout(io_ctx        IN OUT pkg_log_util.gt_log_first,
                            i_fallout_tbl IN VARCHAR2,
                            i_fallout_col IN VARCHAR2,
                            i_base_tbl    IN VARCHAR2,
                            i_expd_cnt    IN NUMBER DEFAULT 0,
                            i_actl_cnt    IN NUMBER DEFAULT 0) IS
  
    v_fallout_id number_var;
    v_diff_cnt   pkg_core.gt_no := 0;
  
  BEGIN
    v_diff_cnt := i_expd_cnt - i_actl_cnt;
  
    IF v_diff_cnt > 0
    THEN
    
      EXECUTE IMMEDIATE ' SELECT ' || i_fallout_col || ' FROM ( SELECT ' || i_fallout_col || ' FROM ' || i_base_tbl ||
                        ' MINUS
                  SELECT ' || i_fallout_col || ' FROM ' || i_fallout_tbl || ' )' BULK COLLECT
        INTO v_fallout_id;
    
      IF pkg_log_util.f_is_info_enabled(io_ctx => io_ctx)
      THEN
      
        pkg_log_util.p_info(io_ctx, 'Fallout detected in load of ' || i_fallout_tbl || '=' || to_char(v_diff_cnt));
      
        FOR i IN 1 .. v_fallout_id.COUNT
        LOOP
          pkg_log_util.p_info(io_ctx, 'Fallout ' || i_fallout_col || ' is: ' || to_char(v_fallout_id(i)));
        END LOOP;
      
      END IF;
    
      v_fallout_id.DELETE;
      raise_application_error(pkg_sir_exceptions.gl_fallout_ex,
                              'Consistency Check: Fallout detected in load of ' || i_fallout_tbl || '=' ||
                               to_char(v_diff_cnt));
    END IF;
  
  END;

END pkg_mtg_srv_util;
/