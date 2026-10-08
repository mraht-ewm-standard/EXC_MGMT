INTERFACE zcx_if_apack_dep_logging
  PUBLIC.

  CLASS-METHODS get_messages_prev
    IMPORTING io_exception       TYPE REF TO cx_root
    RETURNING VALUE(rt_messages) TYPE bapiret2_t.

  CLASS-METHODS get_messages_ext
    IMPORTING io_exception       TYPE REF TO cx_root
    RETURNING VALUE(rt_messages) TYPE bapiret2_t.

  METHODS log_info.

  METHODS log_messages.

  METHODS get_message
    RETURNING VALUE(rs_message) TYPE bapiret2.

  METHODS get_messages
    RETURNING VALUE(rt_messages) TYPE bapiret2_t.

ENDINTERFACE.
