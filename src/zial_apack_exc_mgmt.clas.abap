"! <p class="shorttext synchronized">Metadata: Development</p>
CLASS zial_apack_exc_mgmt DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

ENDCLASS.


CLASS zial_apack_exc_mgmt IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id     = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id  = 'exc-mgmt'.
    if_apack_manifest~descriptor-version      = '01.04.2025.001-rc'.
    if_apack_manifest~descriptor-git_url      = 'https://github.com/mraht-ewm-standard/EXC_MGMT.git' ##NO_TEXT.

  ENDMETHOD.

ENDCLASS.
