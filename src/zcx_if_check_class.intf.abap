INTERFACE zcx_if_check_class
  PUBLIC.

  INTERFACES if_t100_message.
  INTERFACES if_t100_dyn_msg.

  DATA obj_id              TYPE objectname.
  DATA message             TYPE bapiret2.
  DATA messages            TYPE bapiret2_t.
  DATA subrc               TYPE sysubrc.
  DATA input_data          TYPE rsra_t_alert_definition.
  DATA class_name          TYPE classname.
  DATA root                TYPE REF TO zcx_root.

  "! Turn on/off automatic logging
  DATA is_auto_log_enabled TYPE abap_bool.

  METHODS get_message  RETURNING VALUE(rs_message)  TYPE bapiret2.
  METHODS get_messages RETURNING VALUE(rt_messages) TYPE bapiret2_t.

  METHODS log
    IMPORTING iv_with_info TYPE abap_bool DEFAULT abap_true.

  METHODS log_info.

ENDINTERFACE.
