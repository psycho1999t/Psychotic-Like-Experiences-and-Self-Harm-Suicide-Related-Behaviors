# 从公开复现包中移除的内容

本次整理只保留 5 个当前 R 主线脚本。以下内容已从 `08_公开代码整理_20261019` 删除或不再复制：

1. 所有 `.py`、`.ipynb`、`.ps1`、`.sh` 等非 R 文件：按用户要求移除。
2. `00_prepare_panel_data_new_12nodes.R`：严格匹配版本不是当前正式主线。
3. `02_ivpp_gender_comparison_new_12nodes_between_cov.R` 和 `02_ivpp_gender_comparison.R`：历史/候选 IVPP 版本。
4. `inspect_ivpp_structure.R`、`panel_gvar_ivpp_integrated.R`、`panel_ivpp_analysis.R`：结构检查或早期验证脚本，不是最终复现步骤。
5. `03` 至 `09` 等早期绘图脚本：被最终图形脚本替代。
6. 旧版 Panel GVAR 估计脚本、author2 probe 和 panel debug 脚本：历史或调试代码。
7. 报告生成脚本：不属于研究分析复现主线。
8. 外部参考代码、数据、结果、模型对象、图片和手稿：不属于本 R 代码包。

原始项目目录中的上述文件均保留未动；本文件只记录公开代码副本的筛选结果。
