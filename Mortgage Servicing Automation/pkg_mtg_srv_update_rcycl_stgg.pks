CREATE OR REPLACE PACKAGE pkg_mtg_srv_update_rcycl_stgg AS

------------------------------------------------------------------------------
 
  -- Module Name: pkg_mtg_srv_update_rcycl_stgg
  -- Purpose:     This package is used to load the failed transactions into
  --              the staging table to be processed by the TX package.
  -- Stream : Mortgage
  -- Substream : Servicing
  -- File Name:    pkg_mtg_srv_update_rcycl_stgg.pks
-----------------------------------------------------------------------------
  --
  -- Module:      p_load_stgg
  -- Purpose:     Procedure that calls p_load_stgg_data to load staging table. 
  --
  -- Parameters:  i_batch_name           Name of the batch
  --              i_batch_id             Unique ID for this application run.  
  --              i_run_part_id          Unique ID of a run partition within the staging tables. 
  --              i_pcsg_dt              Effective date for the batch.
  --              i_new_tx_threshold     % of io_tx_rec_size above which staging should create new transaction.                    
  --              io_tx_rec_size         Transaction size.
  --                                     If different, it is returned to the staging module.
  --              o_num_total_recs       Total number of records loaded to the primary staging partition
  --              o_num_txs              Total number of transactions generated. 
  --              o_last_tx_size         Size of the last transaction
  --              o_uniform_rec_size_ind Zero (0) indicates transaction record sizes are not uniform 
  --             
  -- Exceptions:  application error when input values are invalid
  -- NOTES:       
  --                         
  ------------------------------------------------------------------------------

  PROCEDURE p_load_stgg(i_batch_name           IN VARCHAR2,
                        i_run_part_id          IN NUMBER,
                        i_batch_id             IN NUMBER,
                        i_pcsg_dt              IN DATE,
                        i_new_tx_threshold     IN NUMBER,
                        io_tx_rec_size         IN OUT NUMBER,
                        o_num_total_recs       OUT NUMBER,
                        o_num_txs              OUT NUMBER,
                        o_last_tx_size         OUT NUMBER,
                        o_uniform_rec_size_ind OUT NUMBER
                       );

END pkg_mtg_srv_update_rcycl_stgg;
/
