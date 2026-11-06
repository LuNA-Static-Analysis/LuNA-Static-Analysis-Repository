import os
from pathlib import Path
from typing import Final

if hybrid_debugger_home := os.getenv('LUNA_HYBRID_DEBUGGER_HOME'):
    HYBRID_DEBUGGER_HOME: Final[Path] = Path(hybrid_debugger_home)
else:
    HYBRID_DEBUGGER_HOME: Final[Path] = Path(__file__).parent

FORMAT_ERRORS_PATH: Final[Path] = HYBRID_DEBUGGER_HOME / 'format-errors.py'

C_ASSERT_CPP_PATH: Final[Path] = HYBRID_DEBUGGER_HOME / '_adapt_assert.cpp'
C_ASSERT_HEADER_PATH: Final[Path] = HYBRID_DEBUGGER_HOME / '_adapt_assert.h'
C_HASH_HEADER_PATH: Final[Path] = HYBRID_DEBUGGER_HOME / '_adapt_hash.h'

REQUIRED_C_FILE_PATHS: Final[tuple[Path, ...]] = (
    C_ASSERT_CPP_PATH,
    C_ASSERT_HEADER_PATH,
    C_HASH_HEADER_PATH,
)
