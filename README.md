# cgt

Go to [Schwab's transaction history](https://client.schwab.com/Apps/accounts/transactionhistory/#/).

# EAC

Select filters

![](schwab-filters.png)

Export with legit JS

```
$(".sch-caret-up").click()


var data='"Txn Date","Action","Symbol","Description","Quanitity","Grant Date","Grant","Date","Price"\n';
var i = 0;
var first = true;
$(".data-row").each(function() {
  $(this).find("td").each(function(j) {
      if (this.innerText !== "") {
        if (!first) {
          data += ',';
        }
        first = false;
        data +=  '"' + this.innerText + '"';
      }
  })
  if (i % 2 == 1) {
    data += "\n";
    first = true;
  }
  i++;
})
```
