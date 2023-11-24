INTERFACE zial_if_exc_group
  PUBLIC.

  CLASS-METHODS is_log_group_enabled
    RETURNING
      VALUE(rv_log_enabled) TYPE cx_bool.
  CLASS-METHODS enable_log_group
    IMPORTING
      iv_log_enabled TYPE abap_bool.

ENDINTERFACE.
