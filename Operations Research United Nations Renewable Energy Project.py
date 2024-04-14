import math
import numpy as np
from scipy.stats import norm
import random
random.seed()


# PS: Peace/Security
def PS(RE, supply, a_1, a_2):
  PS = (1+a_1 * RE) * a_2 * supply
  return PS

# ES: Environmental Sustainability
def ES(RE, supply, a_3):
  ES = a_3*supply/(1+math.exp(-1*RE))
  return ES

# EA: Electricity Access for the Locals = a_4(supply - UN); UN
def EA(UN, supply, a_4):
  EA = a_4*(supply - UN)
  return EA


def FV(lifecycle_capital_costs,lifecycle_om_costs_after_tax, price, supply, analysis_years =25, discount_rate_fraction=0.0638):
    # parameters
    supply = [supply] * analysis_years # anual, 25 yrs
    demand = np.zeros(analysis_years).tolist()
    for yr in range(analysis_years):
      #demand[yr] = [value * random.randint(75, 125) * 0.01 for value in supply[yr]]
      demand[yr] = supply[yr] * random.randint(75, 125) * 0.01
      demand[yr] = min(demand[yr], supply[yr])
    
    # functions
    def cost_function(installation, OM):
      cost = installation + OM
      return cost
    
    cost = cost_function(lifecycle_capital_costs, lifecycle_om_costs_after_tax)
    
    def revenue_function(price, demand): # for one power plant
      revenue = price * demand
      return revenue
    
    
    
    # Risk value sets
    # Exchange rate: a list of 10 values; to USD
    # exchange_rate_set = [0.0137213, 0.0119399, 0.0080733, 0.0067706, 0.0077403, 0.0095502, 0.015548, 0.0077953, 0.008272, 0.0068712]
    er_somalia = [0.0017,0.0018,0.0018,0.0018,0.0018,0.0018,0.0018,0.0019,0.0019,0.0019,0.0019,0.0019,0.0019,0.0019,0.002,0.002,0.002,0.002,0.002,0.002,0.002,0.0021,0.0021,0.0021,0.0021]
    exchange_rate_set = []
    for i in range(10):
      exchange_rate_set.append(random.choice(er_somalia))
    
    # Discount rate: a list of 10 values
    percentage_diff_dr = round((0.0703-discount_rate_fraction)/discount_rate_fraction,4)
    discount_rate_set = [discount_rate_fraction] * 10
    discount_rate_set = [value * random.randint(int(10000*(1-percentage_diff_dr)), int(10000*(1+percentage_diff_dr))) * 0.0001 for value in discount_rate_set]
    
    # Demand: 25*10 list
    demand_set = [demand] * 10
    for i in range(len(demand_set)):
      demand_set[i] = [value * random.randint(95, 105) * 0.01 for value in demand_set[i]]
    
    ## 1000 combinations of NPV
    # each ER * each DR * Demand Values
    revenue_set = []
    revenue_total_discounted = []  # 100 values at the end
    NPV = []  # 1000 values at the end
    
    for exchange_rate in exchange_rate_set:  # 10
        for discount_rate in discount_rate_set:  # 10
            for demand_values in demand_set:  # 25*10
                for j in range(10):  # range(len(demand_set)):
                    discounted_revenue = [revenue_function(price/np.average(er_somalia), demand_values[yr]) / (1 + discount_rate) ** yr for yr in
                                          range(analysis_years)]
                    revenue_set.append(discounted_revenue)
                    revenue_total_discounted.append(sum(discounted_revenue))
                NPV.append(sum(value * exchange_rate for value in revenue_total_discounted))
                revenue_set = []  # Reset the revenue set for the next iteration
                revenue_total_discounted = []  # Reset the discounted revenue set for the next iteration
    
    profit = []
    for i in NPV:
      profit.append(i - cost)
    mean= np.mean(profit)
    variance= np.var(profit)   
    std = np.std(profit)
    #plt.plot(norm.pdf(profit,mean,std))
    #plt.hist(profit)
    #plt.show()
    #print('mean:', mean)
    #print('std:', std)
    return mean, -std

def get_goal(kpi, a_1=1, a_2=1, a_3=1, a_4=1):
    #dict_keys(['price', 'Financial lcc', 'Financial lifecycle_capital_costs', 'Financial offgrid_microgrid_lcoe_dollars_per_kwh', 'Financial lifecycle_om_costs_after_tax', 'Site annual_renewable_electricity_kwh', 'Site total_renewable_energy_fraction', 'PV size_kw', 'PV annual_energy_produced_kwh', 'Generator size_kw', 'Generator annual_fuel_consumption_gal', 'Generator annual_energy_produced_kwh', 'ElectricLoad annual_calculated_kwh', 'ElectricLoad offgrid_load_met_fraction', 'ElectricLoad offgrid_annual_oper_res_required_series_kwh(annual)', 'ElectricLoad offgrid_annual_oper_res_provided_series_kwh(annual)', 'supply', 'UN']
    goal = []
    goal.append(PS(kpi['Site total_renewable_energy_fraction'], kpi['supply'], a_1, a_2))
    goal.append(ES(kpi['Site total_renewable_energy_fraction'], kpi['supply'], a_3))
    goal.append(EA(kpi['UN'], kpi['supply'], a_4))
    mean, std = FV(kpi['Financial lifecycle_capital_costs'],kpi['Financial lifecycle_om_costs_after_tax'],kpi['price'], kpi['supply'])
    goal.append(mean)
    goal.append(std)
    return np.array(goal)
