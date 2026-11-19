"""datasanity: check a dataset for signs it was generated at random rather than recorded."""

from .checks import ALL_CHECKS, CheckResult
from .report import SanityReport, check_dataframe, check_file

__all__ = ["ALL_CHECKS", "CheckResult", "SanityReport", "check_dataframe", "check_file"]
__version__ = "1.0.0"
