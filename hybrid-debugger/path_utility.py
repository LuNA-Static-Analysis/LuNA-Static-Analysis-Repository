from pathlib import Path


def resolve_relative_to(path: Path, root: Path) -> Path:
    if path.is_absolute():
        return path

    return root.joinpath(path)
