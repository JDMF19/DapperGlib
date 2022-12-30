
![DapperGlib](https://github.com/JDMF19/DapperGlib/blob/master/img/logo_completo.png?raw=true)

### Introduction

This [Dapper](https://github.com/DapperLib/Dapper "Dapper repository") extension will allow you to map the database in an easy way, inspired by [Laravel Eloquent](https://laravel.com/docs/9.x/eloquent "eloquent"). 
Has Relationships, Query Builder, Basic Where Clauses, Advanced Where Clauses, Ordering and more.
If you have any questions, suggestions or bugs, please don't hesitate to [contact me](mailto:martinez19florez@gmail.com) or create an issue.

***
### Download

> Install-Package DapperGlib

### Configuration
in the **appsettings.json** file placed in the root folder of your startup project you must define the connection string

```Json
{
  "ConnectionStrings": {
    "SqlConnection": "Your connection string"
  }
}

```
You can use multiple connections just adding another line like this:   


```Json
{
  "ConnectionStrings": {
    "SqlConnection": "Your connection string",
    "AnotherSqlConnection": "Your connection string",
  }
}
```

## Getting Started

### #Model Conventions
As I mentioned before DapperGlib is inspired by Eloquent Laravel, so we will be working with Models. let's examine a basic model class and explain each part.

```C#
using DapperGlib;

namespace App.Models
{
    public class User : Model<User>
    {
        public User()
        {
        }
    }
}
```

The connection string by default is called **SqlConnection** if you use multiple connections or rename default connection string name you must specify it on Models


```C#
using DapperGlib;

namespace App.Models
{
    public class User : Model<User>
    {
        public override string Connection { get; set; } = "AnotherSqlConnection";
    
        public User()
        {
        }
    }
}
```


#### #Table Names
After looking at the example above, you may have noticed that we didn't specify which database table corresponds to our **User** Model. This is because the name of the Class will be used as the name of the table **unless** another name is explicitly specified. So in this case it will be assumed that the **User** model stores records in the User table

If your model's corresponding database table does not fit this convention, you may manually specify the model's table name by defining a table property on the model:

```C#
public class User : Model<User>
{
    [TableName]
    public override string? Table => "Users";
    ...
}
```
> **Note that the TableName attribute must be used on the Property**

#### #Table Schemas
If the table is within a schema it must be specified by overriding the Schema property and using the Schema attribute.

```C#
public class User : Model<User>
{
    [Schema]
    public override string? Schema => "UserSchema";
    ...
}
```

#### #Primary Keys
Unlike the table name, the primary key property must always be specified. The primary key will be used for action queries like Update, Find, Delete, Etc.
We need to use the **PrimaryKey** Attribute on the Property.

```C#
public class User : Model<User>
{
    [PrimaryKey]
    public int UserId { get; set; }
    ...
}
```

In addition, We assumes that the primary key is an incrementing integer value.  
If you wish to use a non-incrementing or a non-numeric primary key you must define a public Incrementing property on your model that is set to false

```C#
public class User : Model<User>
{
    public override bool Incrementing => false;
    ...
}
```


#### #Fillable Properties
Properties that are to be filled and are defined in the Model table must be specified with the **Fillable** attribute.

```C#
public class User : Model<User>
{
    [Fillable]
    public string Name { get; set; }
    ...
}
```

### #Inserting & Updating Models
Once you have created a model and its associated database table, you are ready to start interacting with data from your database. You can think of each **Model** as a query builder allowing you to fluently query the database table associated with the model.

#### #Insert
Now insert records will be very simple.  
To insert a new record into the database, you should instantiate a new Model instance and set attributes on the Model. Then, call the **Insert** method on the Model instance:

```C#
User UserToSave = new();

UserToSave.Name = "Keanu";
UserToSave.LastName = "Reeves";

UserToSave.Insert();
```

You can keep it simpler by using:

```C#
User NewUser = User.Create(new()
{
    Name = "Keanu",
    LastName = "Reeves"
});
```

#### #Update
You can use the **Update** function to update an existing Model in the database.  
The Update function will create a query based on **ALL Fillable Properties** and the **Primary Key** and update the record in the database.  
Following the example above:

```C#

NewUser.Name = "Tom";
NewUser.LastName = "Hanks";
NewUser.Update(); 

```
If you want to update only specific properties you can pass an anonymous object to the Update method
```C#
NewUser.Update(new
{
    LastName = "Holland",
});

```

### #Retrieving Models

#### #Retrieving All Rows From A Table
Of course you can get data from your database using very simple functions.  
The model's **ToList** method will retrieve all of the records from the model's associated database table:

```C#
List<User> Users = User.ToList();
```

#### #Retrieving A Single Row / Column From A Table
If you just need to retrieve a single row from a database table, you may use **First** method:

```C#
User user = User.First();
```


#### #Find a record in Table
If you just need to find a single row from a database table using **Primary Key*, you may use **Find** method:

```C#
User user = User.Find(10);
```
and like this, you have others versions of those methods

* First
* FirstOrDefault
* FirstAsync
* FirstOrDefaultAsync
* Find
* FindAsync
* FindOrDefault
* FindOrDefaultAsync
* ToList
* ToListAsync

### #Deleting Models
You can delete a record using the **Delete** function

```C#
NewUser.Delete();
```

## Query Builder
Query Builder provides a convenient fluent interface for creating and executing database queries. It can be used to perform most database operations.

### #Basic Where Clauses

#### #Where Clauses
You may use the query builder's where method to add "where" clauses to the query. The most basic call to the where method requires three arguments. The first argument is the name of the column. The second argument is an operator, which can be any of the database's supported operators. The third argument is the value to compare against the column's value.

```C#
var user = User.Where("Name", "=", "Tom").First();
```
If you want to verify that a column is `=` to a given value, you may pass the value as the second argument to the where method.  
The Query Builder will assume you would like to use the `=` operator:

```C#
var user = User.Where("Name", "Tom").First();
```

As previously mentioned, you may use any operator that is supported by your database system:

```C#
var user = User.Where("Name", "like", "%Tom%").First();

var users = User.Where("Votes", ">=", 10).ToList();
```

#### #Or Where Clauses
When chaining together calls to the query builder's where method, the "where" clauses will be joined together using the and operator.  
However, you may use the orWhere method to join a clause to the query using the or operator. The orWhere method accepts the same arguments as the where method:

```C#
var users = User.Where("Name","Tom").OrWhere("Name", "Keanu").ToList();
```

#### #Additional Where Clauses

> WhereBetween / WhereNotBetween / WhereDateBetween

The **WhereBetween** method verifies that a column's value is between two values.  
Receives two parameters, the first is the column's name and the second is an instance of the **DapperGlib.Util.Between** class which only has **From** and **To** properties

```C#
var users = User.WhereBetween("Votes", new() { From = 5, To = 10 }).ToList();
```

The **WhereNotBetween** method verifies that a column's value lies outside of two values:

```C#
var users = User.WhereNotBetween("Votes", new() { From = 5, To = 10 }).ToList();
```

The **WhereDateBetween** method verifies that a column's value is between two Dates.  
Receives two parameters, the first is the column's name and the second is an instance of the **DapperGlib.Util.DateBetween** class which only has **From** and **To** properties

```C#
var users = User.WhereDateBetween("Birthday", new() { From = "1956-07-09", To = "1996-06-01" }).ToList();
```

> WhereNot / OrWhereNot  

The **WhereNot** and **OrWhereNot** methods may be used to negate a given group of query constraints

```C#
var users = User.WhereNot((Builder) =>
{
    return Builder.Where("Votes", ">", 10);
}).ToList();
```

> WhereIn / WhereNotIn

The **WhereIn** method verifies that a given column's value is contained within the given array object

```C#
var users = User.WhereIn("UserId", new object[] { 2, 3, 4 }).ToList();
```

The **WhereNotIn** method verifies that a given column's value is not contained within the given array object
```C#
var users = User.WhereNotIn("UserId", new object[] { 2, 3, 4 }).ToList();
```

> WhereNull / WhereNotNull

The **WhereNull** method verifies that the value of the given column is NULL:
```C#
var users = User.WhereNull("LastName").ToList();
```

The **WhereNotNull** method verifies that the value of the given column is NOT NULL:

```C#
var users = User.WhereNotNull("LastName").ToList();
```
> WhereDate / WhereYear / WhereMonth / WhereDay

The **WhereDate** method may be used to compare a column's value against a date:

```C#
var users = User.WhereDate("Birthday", "2022-07-01").ToList();
```

The **WhereYear** method may be used to compare a column's value against a specific year:

```C#
var users = User.WhereYear("Birthday", "2022").ToList();
```

The **WhereMonth** method may be used to compare a column's value against a specific month:

```C#
var users = User.WhereMonth("Birthday", "07").ToList();
```

The **WhereDay** method may be used to compare a column's value against a specific day of the month:

```C#
var users = User.WhereDay("Birthday", "01").ToList();
```
> WhereDateDiffYear / WhereDateDiffMonth / WhereDateDiffDay 

The **WhereDateDiffYear** method is used to compare the difference in years between a column of type date and a given date.

**WhereDateDiffYear**, **WhereDateDiffMonth** and **WhereDateDiffDay** receive the same parameters. the first the column, second the date to compare and the difference. 

```C#
var users = User.WhereDateDiffYear("RegistrationDate", "2022/10/01", 3).ToList(); //Users who registered 3 years ago
```
Optionally you can also pass the comparison operator.

```C#
var users = User.WhereDateDiffYear("RegistrationDate", "2022/10/01", ">=", 3).ToList(); 
```

The **WhereDateDiffMonth** method is used to compare the difference in months between a column of type date and a given date.

```C#
var users = User.WhereDateDiffMonth("RegistrationDate", "2022/10/01", 6).ToList(); //Users who registered 6 months ago
```

The **WhereDateDiffDay** method is used to compare the difference in days between a column of type date and a given date.

```C#
var users = User.WhereDateDiffDay("RegistrationDate", "2022/10/01", 12).ToList(); //Users who registered 12 days ago
```

> WhereColumn

The **WhereColumn** method may be used to verify that two columns are equal:

```C#
var users = User.WhereColumn("Name", "LastName").ToList();
```

You may also pass a comparison operator to the whereColumn method:

```C#
var users = User.WhereColumn("Name", "!=", "LastName").ToList();
```

> **Note: The columns must be of the same type**

> WhereRaw

The **WhereRaw** method is used to pass your own query for the conditional.

```C#
var users = User.Query().WhereRaw("name = 'Math' and city = 'London' ").ToList();
```

#### #Logical Grouping

Sometimes you may need to group several "where" clauses within parentheses in order to achieve your query's desired logical grouping. In fact, you should generally always group calls to the orWhere method in parentheses in order to avoid unexpected query behavior. To accomplish this, you may pass a closure to the where method:

```C#
var users = User.Where("Votes", ">", 100).Where((Builder) =>
{
    return Builder.Where("Votes", ">", 10).OrWhere("Name", "Robert Downey Jr.");
}).ToList();
```
> SQL Output: **select * from Users where Votes > 100 and (Votes > 10 or Name = 'Robert Downey Jr.')**

you can do the same with the OrWhere method

```C#
var users = User.Where("Votes", ">", 100).OrWhere((Builder) =>
{
    return Builder.Where("Votes", ">", 10).Where("Name", "Abigail");
}).ToList();
```

> SQL Output: **select * from Users where Votes > 100 or (Votes > 10 and Name = 'Abigail')**

### #Mass Updates
Updates can also be performed against models that match a given query using the update method with passing an anonymous object.  
In this example, all users that have a destination of San Diego will be updated:

```C#
User.Where("Destination", "San Diego").Update(new {
    Delayed = 1
});
```

### #Mass Delete
The query builder's delete method may be used to delete records from the table. You may constrain delete statements by adding "where" clauses before calling the delete method:

```C#

User.Query().Delete(); // Delete All records

User.Where("Name", "Tom").Delete(); // Delete records with Name 'Tom'

```
If you wish to truncate an entire table, which will remove all records from the table and reset the auto-incrementing ID to zero, you may use the Truncate method:

```C#

User.Truncate();

```
### #Select Statements & Grouping

#### #Specifying A Select Clause
You may not always want to select all columns from a database table. Using the `Select` method, you can specify a custom "select" clause for the query:

```C#
var users = User.Select("Name", "Email").ToList();
```

You can create a class to save only the fields you want to select 
Retrieving methods can receive a generic to map the response to a different class than the model

```C#
public class SelectUser
{
    public string Name { get; set; }
    public string UserEmail { get; set; }
}
var users = User.Select("Name", "Email as UserEmail").ToList<SelectUser>();
```
#### #Grouping

##### GroupBy

As you might expect, the `GroupBy` method may be used to group the query results.

```C#
public class CategoriesWithMoreProducts
{
    public int CategoryId { get; set; }
    public int Products { get; set; }
}
var categories = Product.Select("CategoryId","count(*) as Products").GroupBy("CategoryId").ToList<CategoriesWithMoreProducts>();
```
> The GroupBy method should always be used with the Select method!

##### Having
The `Having` method's signature is similar to that of the where method:

```C#
var categories = Product.Select("CategoryId","count(*) as Products").GroupBy("CategoryId").Having("CategoryId","=", 5).ToList<CategoriesWithMoreProducts>();
```

To build more advanced having statements, use the `HavingRaw` method.
```C#
var orders = Order.Select("Department","SUM(Price) as TotalSales").GroupBy("Department").HavingRaw("SUM(price) > 200").ToList();
```

### #Ordering, Limit & Offset

#### #Ordering

The **OrderBy** method allows you to sort the results of the query by a given column. The first argument accepted by the orderBy method should be the column you wish to sort by, while the second argument determines the direction of the sort and may be either asc or desc:

```C#
var users = User.OrderBy("Name").ToList();

var users = User.OrderBy("Name", "DESC").ToList();
```

To sort by multiple columns, you may simply invoke orderBy as many times as necessary:

```C#
var users = User.OrderBy("Name").OrderBy("LastName").ToList();
```

you can also randomize them using method **InRandomOrder**

```C#
var users = User.InRandomOrder().ToList();
```

#### #Limit & Offset
You may use the **Skip** and **Take** methods to limit the number of results returned from the query or to skip a given number of results in the query:

```C#
var users = User.Take(10).ToList(); // returns the top 10 users
var users = User.Skip(10).ToList(); // returns all users skipping the previous 10
var users = User.Skip(10).Take(10).ToList(); // returns the next 10 users skipping the previous 10
```
> The Order and Limit methods should always be at the end of the query. you can't add any kind of conditional after them

### #Conditional Clauses

Sometimes you may want certain query clauses to apply to a query based on another condition.  
For instance, you may accomplish this using the **When** method:

```C#
var users = User.When(4 == 4, (Builder) =>
{
    return Builder.WhereNotNull("LastName");
}).ToList();
```

### #Aggregates
The query builder also provides a variety of methods for retrieving aggregate values like **Count**, **Max**, **Min**, **Avg** and **Sum**. You may call any of these methods after constructing your query:

```C#
double price  = Product.Max("Price");
```
Of course, you may combine these methods with other clauses to fine-tune how your aggregate value is calculated:

```C#
int TotalProductsWithDiscount  = Product.Where("Discount", ">", 0).Count();
double MaxPriceDiscount = Product.Where("Discount", ">", 0).Max("Price");
double TotalPriceDiscount = Product.Where("Discount", ">", 0).Sum("Price");
```
#### #Determining If Records Exist

Instead of using the count method to determine if any records exist that match your query's constraints, you may use the Exists and DoesntExist methods:

```C#

if(Product.Where("Discount", ">", 0).Exists()){
  // ...
}

if(Product.Where("Discount", ">", 0).DoesntExist()){
  // ...
}

```

### #Distinct
The **Distinct** method allows you to force the query to return distinct results:

```C#
var products = Product.Distinct().ToList();
```
> SQL Output: **select DISTINCT * from Product**

You can specify the fields by passing a string separated by commas

```C#
var products = Product.Distinct("Name, Price").ToList();
```
> SQL Output: **select DISTINCT Name, Price from Product**

### #Util

You can get an instance of the QueryBuilder without conditions with the `Query` method:
```C#
var Builder = Product.Query();
```
You can clone an instance of the QueryBuilder using the `Clone` method:

```C#
var Builder1 = Product.Where("CategoryId", 5);
var Builder2 = Builder1.Clone();
```
You can get the name of the table related to the Model with the `GetTableName` method:

```C#
string table = Product.GetTableName();
```
You can get the sql output from the QueryBuilder using the `ToSql` method:

```C#
string sql = Product.Where("CategoryId", 5).ToSql();
```
## Relationships

### #Introduction
Database tables are often related to one another. For example, a blog post may have many comments or an order could be related to the user who placed it. Now you can get the related tables easily.

### #HasRelationship

You can use the HasRelationship method inherited from the Model class to get the relationships, this method receives an instance of the generic **Relationship** class.

The **Relationship** class receives a generic and two arguments. The generic must be the Model to which it is related, the first argument is the local key of the relation and the second is the foreign key. If the local and foreign keys match, only 1 argument can be passed.  

let's see an example.

```C#
using DapperGlib;

namespace App.Models
{

    public class User : Model<User>
    {
        [PrimaryKey]
        public int UserId { get; set; }

        [Fillable]
        public string Name { get; set; }

        [Fillable]
        public string LastName { get; set; }
        
        public Relationship<Comment> Comments => HasRelationship(new Relationship<Comment>(localKey: "UserId", foreignKey: "UserId"));   
        // First **UserId** is the column on User's table, Second is the column on Comment's table
        
        public User()
        {
        }
    }

    public class Comment : Model<Comment>
    {
        [PrimaryKey]
        public int CommentId { get; set; }

        [Fillable]
        public int UserId { get; set; }

        [Fillable]
        public string Text { get; set; }
        
        [Fillable]
        public int Likes { get; set; }
        
        public Comment()
        {
        }
    }
}
```
now you can get the comments of a user in this way

```C#
var user = User.Find(3);
List<Comment> Comments = user.Comments.ToList();
```

Since all relationships also serve as query builders, you may add further constraints to the relationship query.

```C#
var user = User.Find(3);
List<Comment> Comments = user.Comments.Where("Likes", ">", 50).ToList();
```

You can also use the **WithCount** method to get the number of records in a relation

```C#
var users = User.WithCount("Comments").ToList();
```
by default the return column name will be the name of the relation property followed by an underscore and the word Count, in this example it would be: `Comments_Count`
you can change this by passing an alias as the second parameter.

```C#
var users = User.WithCount("Comments","NumComments").ToList();
```

> **Note: The string passed to the WithCount method must be the name of a relationship property in the model**

### #Querying Relationship Existence

When retrieving model records, you may wish to limit your results based on the existence of a relationship. For these cases use **WhereHas** and **OrWhereHas** method

#### #WhereHas
```C#
//Retrieve all users that have at least one comment...
var users = User.WhereHas<Comment>("Comments").ToList();
```
> **Note! The string passed to the WhereHas method must be the name of a relationship property in the model and you need to pass the Model type related**

You may also specify an operator and count value to further customize the query:

```C#
// Retrieve all users that have three or more comments...
var users = User.WhereHas<Comment>("Comments", ">=", 3).ToList();
```
If you need even more power, you may pass a `closure` to define additional query constraints on your queries, such as validate the Likes of a comment:

```C#
// Retrieve all users that have one or more comments with Likes over 100...
var users = User.WhereHas<Comment>("Comments", (Builder)=>{
    return Builder.Where("Likes", ">", 100);
}).ToList();

// Retrieve users with at least ten comments with Likes over 100...
var users = User.WhereHas<Comment>("Comments", (Builder)=>{
    return Builder.Where("Likes", ">", 100);
}, ">=", 10).ToList();
```

#### #OrWhereHas
The OrWhereHas method works in the same way as the WhereHas method but changing the AND operator to OR

```C#
//Retrieve all users that name is 'Tom' OR have at least one comment...
var users = User.Where("Name", "Tom").OrWhereHas<Comment>("Comments").ToList();
```

### #Querying Relationship Absence
When retrieving model records, you may wish to limit your results based on the absence of a relationship. You can use **WhereDoesntHave** and **OrWhereDoesntHave**

#### #WhereDoesntHave
```C#
//Retrieve all users that not have comments...
var users = User.WhereDoesntHave<Comment>("Comments").ToList();
```
you can also pass a `closure` to add an additional constraint

```C#
// Retrieve all users that not have comments with Likes over 100...
var users = User.WhereDoesntHave<Comment>("Comments", (Builder)=>{
    return Builder.Where("Likes", ">", 100);
}).ToList();
```
#### #OrWhereDoesntHave

```C#
//Retrieve all users that name is 'Tom' or not have comments...
var users = User.Where("Name", "Tom").OrWhereDoesntHave<Comment>("Comments").ToList();
```

## Still is Dapper
Of course you can still use dapper's interface by instantiating the context

```C#
using Dapper;
using DapperGlib;

GlipContext _context = new();

using var conection = _context.CreateConnection();
var users = conection.Query<User>("SELECT * FROM Users");

```

### #Transactions
You can use transactions in the same way

```C#
using System.Transactions;


using (var transactionScope = new TransactionScope())
{
    User.Create(new()
    {
        Name = "Patric",
        LastName = "Jane"
    });

    transactionScope.Complete();
}

```
