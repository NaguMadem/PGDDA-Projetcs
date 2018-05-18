#Solution for SQL assignment
use superstoresdb;
# Task:1
# 1.1. Describe the dataset
# The dataset is related the sales. The customer tables stores the all the data related to the customer.
# The Product is set of all the products
# The customers order products is stored in the orders table
# The Market_fact table stores the all the calculations like discount, Sales, order quantity

# 1.2. Identify and List the Primary Keys and Foreign Keys

#customer table
#Primary Key: Cust_Id
#Foreign Key: No FK

# market_fact table
# Primary Key: No Primary Key for this table
# Foreign Key: Ord_id refers to the Ord_id of orders table
# Prod_id refers to Prod_id in product table
# Cust_id refers to Cust_id in customer table
# Ship_id refers to Ship_id in ship table

#orders table
#Primary Key: Ord_id
#Foreign Key: No FK

#products table
#Primary Key: Prod_Id
#Foreign Key: No FK 

#shipment table
#Primary Key: Ship_id
#Foreign Key: No FK

#Task:2      
#Find the total and average sales
describe market_fact;
select sum(sales) from market_fact;
select avg(sales) from market_fact;

#Display the number of customers in each region in decreasing order of no_of_customers. 
#The result should be a table with columns Region, no_of_customers
select Region, count(*) as no_of_customers 
      from cust_dimen
      group by Region
      order by no_of_customers desc;

#Find the region having maximum customers (display the region name and max(no_of_customers)
select Region, count(*) as no_of_customers from cust_dimen group by Region order by no_of_customers DESC limit 1;

#Find the number and id of products sold in decreasing order of products sold (display product id, no_of_products sold) 
select Prod_id, sum(Order_Quantity) as no_of_products_sold from market_fact group by Prod_id order by no_of_products_sold desc;

#Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of tables purchased (display the customer name, no_of_tables purchased)

select Customer_Name, sum(Order_Quantity) as num_tables from market_fact s, cust_dimen c, prod_dimen p where s.Cust_id = c.Cust_id and s.Prod_id = p.Prod_id and 
      p.Product_Sub_Category = 'TABLES' and
      c.Region = 'ATLANTIC'
      group by Customer_Name;
      
#Task:3      
#Display the product categories in descending order of profits (display the product category wise profits i.e. product_category, profits)?
select Product_Category, sum(profit) as Total_profit
      from market_fact s, prod_dimen p
      where s.Prod_id = p.Prod_id
      group by p.Product_Category
      order by Total_profit DESC;

#Display the product category, product sub-category and the profit within each sub-category in three columns.
select Product_Category, Product_Sub_Category, sum(profit) as total_profit
      from market_fact s, prod_dimen p
      where s.Prod_id = p.Prod_id
      group by Product_Category,Product_Sub_Category
      order by total_profit;

#Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, 
# display the  region-wise no_of_shipments and the profit made in each region in decreasing order of profits 
# (i.e. region, no_of_shipments, profit_in_each_region)
select Region, count(*) as no_of_table_shipments, sum(profit) as profit_in_region
      from market_fact s, cust_dimen c, prod_dimen p
      where s.Cust_id = c.Cust_id and
      s.Prod_id = p.Prod_id and
      p.Product_Sub_Category = 'TABLES'
      group by Region
      order by profit_in_region desc;

