# 研究复现代码（R）

本仓库提供数据准备、正式 IVPP/panelGVAR 分析和图形生成的 R 脚本。为保护参与者隐私，仓库不含原始/整理后数据、模型对象、结果表、图形或手稿；所有输入需由使用者在本地准备。

## 运行顺序

1. **数据准备**：运行 `01_data_preparation/00_prepare_panel_data_new_12nodes_between_simplified.R`。输入须为已完成三波纵向匹配的长格式数据，包含 `id_key`、`wave`、`gender`、`pe1–pe8`、`si1–si4`。设置 `PANEL_INPUT_PATH` 指向该文件；可选设置 `PANEL_OUTPUT_BASE` 指定输出位置。匹配步骤原先仅有 Python 实现，因此本包的复现起点是已匹配数据。
2. **正式分析**：运行 `02_formal_ivpp_analysis/02_ivpp_gender_comparison_author_style_12nodes.R`。设置 `PREPARED_DATA_RDS` 指向步骤 1 输出的 `prepared_panel_data.rds`；可选设置 `IVPP_OUTPUT_BASE` 和 `IVPP_OUTPUT_TAG`。脚本进行作者口径 IVPP/panelGVAR、global invariance 和独立 partial-pruning 分析。
3. **图形**：运行 `03_visualization/` 中脚本。Figure 2/3 脚本依赖已整理的分析结果表和描述性统计文件；将其放入本地运行目录结构 `inputs/results/`、`inputs/descriptive_numeric.csv`，或设置 `IVPP_RUNTIME_DIR` 指向包含这些 `inputs/` 的目录。Figure 4 需另行提供获准公开的图形源数据 CSV，并设置 `FIGURE4_DATA_PATH`；图形写入当前工作目录。

建议从仓库根目录运行脚本，以便默认相对输出路径保持一致。涉及外部数据和结果时优先使用环境变量传入路径；不要将参与者级数据、受限结果或本机路径清单提交到仓库。所需 R 包请按脚本中的 `library()` 调用安装，IVPP 分析另需 `IVPP` 包。

## 目录

- `01_data_preparation/`：已匹配三波长格式数据的整理与质控。
- `02_formal_ivpp_analysis/`：正式 IVPP/panelGVAR 估计和模型比较。
- `03_visualization/`：主图和补充图生成。
- `04_documentation/`：代码清单及排除范围说明。

代码清单记录公开副本文件名、用途及 SHA-256；不记录个人电脑上的原始路径。被排除内容见 `04_documentation/REMOVED_FROM_PACKAGE.md`。
