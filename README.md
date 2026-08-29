# CoEVaR^SIM

This repository provides the code and data associated with the paper:

> Zhang, F., Xu, Y., & Chen, Y. (2026).  
> **A study of nonlinear tail risk measurement and spillover effects in equity markets under extreme events.**  
> *Systems Engineering — Theory & Practice*, 46(2), 618–637.  
> DOI: 10.12011/SETP2023-2712

## Overview

This repository provides the implementation of the nonlinear conditional expectile value-at-risk (**CoEVaR-SIM**) framework proposed in the paper.

The method combines the **single-index expectile model (SIEM)** with **conditional expectile value-at-risk (CoEVaR)** to capture nonlinear tail-risk spillovers across financial markets. Based on CoEVaR-SIM, dynamic tail-risk spillover networks are constructed to investigate the magnitude, direction, and structure of risk transmission across global equity markets.

## Data

The empirical analysis uses weekly returns of 30 major global stock market indices from June 2009 to June 2023.

The stock markets are grouped into three regions:

- Americas
- Europe
- Asia

The analysis also incorporates macro-financial variables, including exchange rates, the VIX index, commodity futures, and energy prices.

The data used for replication are provided in the `data/` folder.

## Repository Structure

```text
CoEVaR-SIM/
├── README.md
├── code/
│   ├── CoEVaR/
│   ├── CoEVaR_SIM/
│   ├── CoEVaR_SIM_and_Linear/
│   ├── CoEVaR_mst_SIM/
│   ├── CoEVaR_network_SIMplot/
│   ├── CoEVaR_Robust_SIM/
│   ├── CoEVaR_SIFIs/
│   └── CoEVaR_SIFIs_SIM/
├── data/
└── results/
```

## Main Folders

- **`code/CoEVaR/`**  
  Estimation of the linear CoEVaR model.

- **`code/CoEVaR_SIM/`**  
  Estimation of the nonlinear CoEVaR-SIM model.

- **`code/CoEVaR_SIM_and_Linear/`**  
  Comparison between the nonlinear CoEVaR-SIM model and the linear CoEVaR model.

- **`code/CoEVaR_mst_SIM/`**  
  Maximum spanning tree analysis of the CoEVaR-SIM tail-risk spillover network.

- **`code/CoEVaR_network_SIMplot/`**  
  Visualization of tail-risk spillover networks.

- **`code/CoEVaR_Robust_SIM/`**  
  Robustness analysis for the CoEVaR-SIM model.

- **`code/CoEVaR_SIFIs/`**  
  Systemic risk analysis based on the linear CoEVaR model.

- **`code/CoEVaR_SIFIs_SIM/`**  
  Systemic risk analysis based on the nonlinear CoEVaR-SIM model.

- **`data/`**  
  Data used in the empirical analysis.

- **`results/`**  
  Selected intermediate and empirical results.

## Methodology

The empirical analysis consists of the following main steps:

1. **EVaR estimation**  
   Estimate the expectile value-at-risk (EVaR) for each stock market.

2. **CoEVaR-SIM estimation**  
   Estimate nonlinear conditional tail risk using the single-index expectile model.

3. **Tail-risk spillover measurement**  
   Measure directional tail-risk spillovers using the local gradient of the estimated single-index function.

4. **Dynamic network construction**  
   Construct weighted and directed dynamic tail-risk spillover networks based on the estimated spillover intensities.

5. **Systemic risk analysis**  
   Calculate risk receiving, risk spilling, net spillover, and system-wide spillover measures.

6. **Network structure analysis**  
   Use the maximum spanning tree to identify the major transmission channels and central nodes in the tail-risk spillover network.

The dynamic network analysis is conducted using a **48-week rolling window**.

## Software

The empirical analysis is implemented primarily in **R**.

Required R packages should be installed before running the corresponding scripts.

## Citation

If you use the code or methodology provided in this repository, please cite:

> Zhang, F., Xu, Y., & Chen, Y. (2026).  
> A study of nonlinear tail risk measurement and spillover effects in equity markets under extreme events.  
> *Systems Engineering — Theory & Practice*, 46(2), 618–637.  
> https://doi.org/10.12011/SETP2023-2712

## Contact

**Yixiong Xu**  
School of Economics and Finance  
Xi'an Jiaotong University  

Email: xyx123@stu.xjtu.edu.cn
  
