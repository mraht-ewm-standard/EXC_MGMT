"! <p class="shorttext synchronized" lang="en">Metadata: Development</p>
CLASS zial_apack_exc_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zial_apack_exc_mgmt IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id    = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id = 'ewm-exc-mgmt'.
    if_apack_manifest~descriptor-version     = '12.12.2022.001-rc'.
    if_apack_manifest~descriptor-git_url     = 'https://github.com/mraht-ewm-standard/EXC_MGMT.git'.

  ENDMETHOD.

ENDCLASS.
