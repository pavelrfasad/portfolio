(function () {
 'use strict';
 const {Store,number,xlsx}=CuttingData,store=new Store();
 const host=document.getElementById('materials-table'),status=document.getElementById('table-status');
 let sheet='spec',hidden=new Set(),editingInput=null;
 const common=[['group','Группа'],['name','Наименование'],['label','Марка / размер'],['color','Цвет'],['unit','Ед.']];
 const specification=[...common,['projectQty','По проекту'],['waste','Запас, %'],['size','Кратность'],['qty','К закупке'],['cutMap','По раскрою'],['perSqM','На м²']];
 const quotation=[...common,['qty','Количество'],['markup','Наценка, %'],['price','Цена'],['total','Сумма']];
 const numeric=new Set(['projectQty','waste','size','qty','markup','price','factor']);
 const fmt=n=>new Intl.NumberFormat('ru-RU',{maximumFractionDigits:6}).format(number(n));
 const node=(tag,text,cls)=>{const el=document.createElement(tag);if(text!==undefined)el.textContent=text;if(cls)el.className=cls;return el;};
 function button(label,action,title){const b=node('button',label);b.type='button';b.title=title||label;b.addEventListener('click',action);return b;}
 function message(text){status.textContent=text;}
 function columns(){const fields=[...(sheet==='spec'?specification:quotation)];if(store.rows().some(r=>r.baseId))fields.splice(5,0,['factor','На ед. базы']);return fields.filter(([field])=>!hidden.has(field));}
 function commit(id,field,value){try{store.update(id,field,numeric.has(field)?number(value):value);}catch(error){message(error.message);}}
 function render(){
  const rows=store.rows(),cols=columns();host.replaceChildren();
  document.querySelectorAll('[data-sheet]').forEach(b=>{const selected=b.dataset.sheet===sheet;b.setAttribute('aria-selected',selected);b.classList.toggle('selected',selected);});
  document.getElementById('table-empty').hidden=rows.length>0;
  const table=node('table');table.className='data-table';const thead=node('thead'),head=node('tr');head.append(node('th','Вкл.'));
  cols.forEach(([,name])=>head.append(node('th',name)));head.append(node('th','Действия'));thead.append(head);table.append(thead);
  const tbody=node('tbody');
  for(const row of rows){
   const tr=node('tr');tr.dataset.rowId=row.id;if(row.state==='inactive')tr.className='inactive';
   const active=node('td'),check=node('input');check.type='checkbox';check.checked=row.state!=='inactive';check.setAttribute('aria-label','Включить '+(row.name||row.label||'строку'));check.addEventListener('change',()=>commit(row.id,'state',check.checked?'active':'inactive'));active.append(check);tr.append(active);
   for(const [field]of cols){
    const td=node('td');td.dataset.field=field;
    const computed=['total','perSqM','cutMap'].includes(field)||(field==='projectQty'&&(row.source==='dxf'||row.baseId))||(field==='factor'&&!row.baseId);
    if(computed){td.textContent=field==='cutMap'?(row.cutMap||'—'):field==='factor'?'—':fmt(row[field]);}
    else{
     const input=node('input');input.type=numeric.has(field)?'number':'text';if(input.type==='number'){input.step='any';if(field!=='markup')input.min='0';}
     input.value=row[field]??'';input.setAttribute('aria-label',(cols.find(c=>c[0]===field)?.[1]||field)+' — '+(row.name||row.label||'строка'));
     input.addEventListener('input',()=>{editingInput=input;try{commit(row.id,field,input.value);}finally{editingInput=null;}});input.addEventListener('change',()=>commit(row.id,field,input.value));td.append(input);
     if(field==='qty'&&row.qtyOverride!=null)td.append(button('Авто',()=>commit(row.id,'qtyOverride',null),'Вернуть расчётное количество'));
    }
    if(field==='name'&&row.baseId){const base=rows.find(r=>r.id===row.baseId);td.append(node('small','Связано: '+(base?.name||base?.label||'строка')));}
    tr.append(td);
   }
   const actions=node('td',undefined,'row-actions');actions.append(button('↳',()=>store.add(row.id),'Добавить связанную строку'),button('↑',()=>store.move(row.id,-1),'Переместить вверх'),button('↓',()=>store.move(row.id,1),'Переместить вниз'),button('×',()=>store.remove(row.id),'Удалить строку'));tr.append(actions);tbody.append(tr);
  }
  table.append(tbody);host.append(table);
  renderSummary(rows);renderColumns();
 }
 function renderSummary(rows){
  const enabled=rows.filter(r=>r.state!=='inactive'),totals=new Map();enabled.forEach(r=>totals.set(r.unit||'ед.',(totals.get(r.unit||'ед.')||0)+r.qty));
  document.getElementById('table-total').textContent=sheet==='quote'?'Итого: '+fmt(enabled.reduce((sum,r)=>sum+r.total,0))+' '+store.header.currency:'К закупке: '+[...totals].map(([unit,qty])=>fmt(qty)+' '+unit).join(' · ');
 }
 function refreshCalculated(rows){
  const byId=new Map(rows.map(r=>[r.id,r]));
  for(const tr of host.querySelectorAll('tbody tr')){
   const row=byId.get(tr.dataset.rowId);if(!row)continue;
   for(const td of tr.querySelectorAll('[data-field]')){
    const field=td.dataset.field,input=td.querySelector('input');
    if(input&&input!==editingInput&&['qty','projectQty'].includes(field))input.value=row[field]??'';
    if(!input&&['total','perSqM','projectQty'].includes(field))td.textContent=fmt(row[field]);
   }
  }
  renderSummary(rows);
 }
 function renderColumns(){const options=document.getElementById('column-options');options.replaceChildren();const fields=sheet==='spec'?specification:quotation;for(const [field,name]of fields){const label=node('label'),check=node('input');check.type='checkbox';check.checked=!hidden.has(field);check.addEventListener('change',()=>{if(check.checked)hidden.delete(field);else hidden.add(field);render();});label.append(check,document.createTextNode(name));options.append(label);}}
 function setHeader(){document.getElementById('project-title').value=store.header.title;document.getElementById('facade-area').value=store.header.area||'';document.getElementById('quote-currency').value=store.header.currency;}
 store.onChange=rows=>editingInput?refreshCalculated(rows):render();
 document.querySelectorAll('[data-sheet]').forEach(b=>b.addEventListener('click',()=>{sheet=b.dataset.sheet;render();}));
 document.getElementById('project-title').addEventListener('input',e=>store.header.title=e.target.value);
 document.getElementById('facade-area').addEventListener('change',e=>{store.header.area=Math.max(0,number(e.target.value));render();});
 document.getElementById('quote-currency').addEventListener('change',e=>{store.header.currency=e.target.value;render();});
 document.getElementById('add-row').addEventListener('click',()=>store.add());
 document.getElementById('show-all').addEventListener('click',()=>{hidden.clear();render();});
 function download(data,name,type){const url=URL.createObjectURL(new Blob([data],{type})),a=node('a');a.href=url;a.download=name;document.body.append(a);a.click();a.remove();setTimeout(()=>URL.revokeObjectURL(url),1000);}
 document.getElementById('export-json').addEventListener('click',()=>download(JSON.stringify(store.snapshot(),null,2),'cutting-project.json','application/json'));
 document.getElementById('import-json').addEventListener('change',async e=>{try{const file=e.target.files[0];if(!file)return;store.restore(JSON.parse(await file.text()));setHeader();message('Проект таблицы восстановлен из JSON. Для нового раскроя загрузите соответствующий DXF.');}catch(error){message('Не удалось загрузить проект: '+error.message);}finally{e.target.value='';}});
 function sheetRows(name,fields){const rows=store.rows().filter(r=>r.state!=='inactive');return {name,rows:[[store.header.title||'Материалы проекта'],['Площадь фасада, м²',number(store.header.area)],['Валюта',store.header.currency],fields.map(c=>c[1]),...rows.map(r=>fields.map(([field])=>r[field]??''))]};}
 document.getElementById('export-excel').addEventListener('click',()=>{try{download(xlsx([sheetRows('Спецификация',specification),sheetRows('КП',quotation)]),'specification.xlsx','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');message('Экспортированы обе вкладки. Отключённые строки исключены.');}catch(error){message('Ошибка экспорта: '+error.message);}});
 document.getElementById('export-pdf').addEventListener('click',()=>{
  const area=document.getElementById('table-print');area.replaceChildren(node('h1',store.header.title||'Материалы проекта'),node('p',(sheet==='spec'?'Спецификация':'Коммерческое предложение')+' · '+store.header.currency));
  const t=node('table'),h=node('tr'),cols=columns();cols.forEach(([,name])=>h.append(node('th',name)));const th=node('thead');th.append(h);t.append(th);const tb=node('tbody');for(const r of store.rows().filter(r=>r.state!=='inactive')){const tr=node('tr');cols.forEach(([field])=>tr.append(node('td',typeof r[field]==='number'?fmt(r[field]):r[field]||'')));tb.append(tr);}t.append(tb);area.append(t,node('p',document.getElementById('table-total').textContent));document.body.dataset.printMode='table';window.print();
 });
 window.CuttingTable={
  setDxfRows(rows){store.setBase(rows);message(rows.length?'Данные DXF загружены. После раскроя количество обновится по фактическим заготовкам.':'В DXF нет подходящих элементов. Можно добавить строки вручную.');},
  setCutRows(rows){store.setCuts(rows);message('Раскрой завершён: количество к закупке рассчитано по использованным листам и пруткам. Запас задаётся отдельно.');},
  resetCutRows(){store.resetCuts();message('Результаты раскроя сброшены. Показаны исходные количества.');},
  clear(){store.clear();message('Загрузите DXF или добавьте строки вручную.');},
  getData(){return store.rows().map(r=>({...r}));}
 };
 setHeader();render();
})();
