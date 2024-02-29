INTERFACE zcx_if_check_class
  PUBLIC.

  INTERFACES if_t100_message.
  INTERFACES if_t100_dyn_msg.

  DATA obj_id     TYPE objectname.
  DATA message    TYPE bapiret2.
  DATA messages   TYPE bapiret2_t.
  DATA subrc      TYPE sysubrc.
  DATA input_data TYPE rsra_t_alert_definition.

  METHODS get_message  RETURNING VALUE(rs_message)  TYPE bapiret2.
  METHODS get_messages RETURNING VALUE(rt_messages) TYPE bapiret2_t.

ENDINTERFACE.
