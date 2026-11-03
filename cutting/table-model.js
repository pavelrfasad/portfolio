(function (root) {
 'use strict';
 const number = value => { const n = Number(String(value ?? '').replace(/\s/g, '').replace(',', '.').replace(/%$/, '')); return Number.isFinite(n) ? n : 0; };
 const positive = value => Math.max(0, number(value));
 const round = n => Math.round((n + Number.EPSILON) * 1e6) / 1e6;
 const sourceKey = row => JSON.stringify([row.material || row.name || row.label || '', row.color || '', row.unit || '']);
 const materialKey = row => JSON.stringify([row.material || row.name || row.label || '', row.color || '']);
 const xml = value => String(value ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&apos;'}[c]));
 function parseDxf(text) {
  const lines = text.replace(/^\uFEFF/, '').split(/\r?\n/);
  const records = []; let record;
  for (let i = 0; i + 1 < lines.length; i += 2) {
   const code = lines[i].trim(), value = lines[i + 1].trim();
   if (code === '0') { record = {type:value, start:i, fields:[]}; records.push(record); }
   else if (record) record.fields.push([code,value]);
  }
  const blocks = Object.create(null), layers = Object.create(null);
  const get = (r,c,f='') => r.fields.find(x => x[0] === c)?.[1] ?? f;
  const layer = name => layers[name] ||= {length:0, blockNames:[], blocksWithoutAttrs:0, hasLinearBlocks:true};
  let section = '', insertion = null, insertLayer = '';
  for (const r of records) {
   if (r.type === 'SECTION') {section=get(r,'2'); insertion=null;continue;}
   if (r.type === 'ENDSEC') {section='';insertion=null;continue;}
   if (section !== 'ENTITIES') continue;
   if (r.type === 'INSERT') {
    insertLayer = get(r,'8','0'); const l = layer(insertLayer);
    const key = 'insert-' + r.start;
    insertion = {attributes:[], attribIndex:[], name:get(r,'2'), key};
    (blocks[insertLayer] ||= Object.create(null))[key] = insertion;
    l.blockNames.push(key);
   } else if (r.type === 'ATTRIB' && insertion) {
    const tag=get(r,'2'); if (tag) {insertion.attributes.push({tag,value:get(r,'1')}); insertion.attribIndex.push(r.start);}
   } else if (r.type === 'SEQEND') insertion=null;
   else {
    insertion=null;
    if (r.type === 'LINE') {
     const l=layer(get(r,'8','0'));
     l.length += Math.hypot(number(get(r,'11'))-number(get(r,'10')),number(get(r,'21'))-number(get(r,'20')))/1000;
    } else if (r.type === 'LWPOLYLINE') {
     const vertices=[]; let v;
     for (const [code,value] of r.fields) {
      if(code==='10'){v={x:number(value),y:0,bulge:0};vertices.push(v);}
      if(code==='20'&&v)v.y=number(value);
      if(code==='42'&&v)v.bulge=number(value);
     }
     let length=0, count=vertices.length-1;
     if (number(get(r,'70')) & 1) count++;
     for(let i=0;i<count;i++){
      const a=vertices[i],b=vertices[(i+1)%vertices.length];if(!a||!b)continue;
      const chord=Math.hypot(a.x-b.x,a.y-b.y),bulge=Math.abs(a.bulge);
      length += bulge>1e-10 ? chord*(1+bulge*bulge)/(4*bulge)*Math.abs(4*Math.atan(bulge)) : chord;
     }
     layer(get(r,'8','0')).length += length/1000;
    }
   }
  }
  for (const [name,l] of Object.entries(layers)) {
   const withSize=[];
   for(const key of l.blockNames){const b=blocks[name][key];if(b.attributes.some(a=>a.tag.toLowerCase()==='размер'))withSize.push(key);else l.blocksWithoutAttrs++;}
   // Marking-only blocks are kept for marking, but do not get cutting controls.
   l.blockNames = l.blockNames.filter(key=>blocks[name][key].attributes.length);
  }
  return {lines,blocks,layers};
 }
 class Store {
  constructor(){this.base=[];this.cuts=[];this.manual=[];this.edits=new Map();this.hidden=new Set();this.header={title:'',area:0,currency:'RUB'};this.serial=0;this.order=[];this.onChange=()=>{};}
  changed(){this.onChange(this.rows());}
  setBase(rows){this.base=rows.map(r=>({...r,material:r.material||r.name||r.label||''}));this.cuts=[];this.changed();}
  setCuts(rows){this.cuts=rows.map(r=>({...r,material:r.material||r.name||r.label||''}));for(const e of this.edits.values())delete e.qtyOverride;this.changed();}
  resetCuts(){this.cuts=[];for(const e of this.edits.values())delete e.qtyOverride;this.changed();}
  clear(){this.base=[];this.cuts=[];this.manual=[];this.edits.clear();this.hidden.clear();this.order=[];this.changed();}
  sourceRows(){const cutMaterials=new Set(this.cuts.map(materialKey));return [...this.base.filter(r=>!cutMaterials.has(materialKey(r))),...this.cuts];}
  rows(){
   const rows=this.sourceRows().map(r=>{const id='dxf:'+sourceKey(r);return {...r,id,source:'dxf',waste:0,...this.edits.get(id)};}).filter(r=>!this.hidden.has(r.id));
   rows.push(...this.manual.map(r=>({...r})));const map=new Map(rows.map(r=>[r.id,r])),done=new Set(),active=new Set();
   const calculate=row=>{
    if(done.has(row.id))return;
    if(active.has(row.id))throw new Error('Циклическая связь строк');active.add(row.id);
    row.projectQty=positive(row.projectQty);row.waste=positive(row.waste);row.size=positive(row.size);
    if(row.baseId){const base=map.get(row.baseId);if(base){calculate(base);row.projectQty=base.state==='inactive'?0:base.qty*positive(row.factor);}else row.projectQty=0;}
    const baseQty=row.cutQty==null?row.projectQty:positive(row.cutQty);
    let qty=baseQty*(1+row.waste/100);
    if(row.size>0)qty=Math.ceil((qty-1e-9)/row.size)*row.size;
    row.qty=row.qtyOverride==null?round(qty):positive(row.qtyOverride);
    row.perSqM=positive(this.header.area)?round(row.qty/positive(this.header.area)):0;
    row.price=positive(row.price);row.markup=number(row.markup);
    row.total=Math.round(row.qty*row.price*(1+row.markup/100)*100)/100;
    row.state ||= 'active';done.add(row.id);active.delete(row.id);
   };
   rows.forEach(calculate);const positions=new Map(this.order.map((id,i)=>[id,i]));return rows.sort((a,b)=>(positions.get(a.id)??Infinity)-(positions.get(b.id)??Infinity));
  }
  update(id,field,value){const manual=this.manual.find(r=>r.id===id);const target=manual||(this.edits.get(id)||{});if(field==='qty')field='qtyOverride';target[field]=value;if(['projectQty','waste','size','factor'].includes(field))delete target.qtyOverride;if(!manual)this.edits.set(id,target);this.changed();}
  add(baseId){const row={id:'manual-'+(++this.serial),source:'manual',state:'active',group:'Дополнительные материалы',name:'',label:'',color:'',unit:'шт',projectQty:0,waste:0,size:0,price:0,markup:0};if(baseId){row.baseId=baseId;row.factor=1;row.group='Сопутствующие материалы';}this.manual.push(row);this.changed();return row.id;}
  remove(id){const remove=new Set([id]);let previous=-1;while(previous!==remove.size){previous=remove.size;this.manual.forEach(r=>{if(remove.has(r.baseId))remove.add(r.id);});}this.manual=this.manual.filter(r=>!remove.has(r.id));for(const key of remove){this.hidden.add(key);this.edits.delete(key);}this.changed();}
  move(id,delta){const ids=this.rows().map(r=>r.id),i=ids.indexOf(id),j=i+delta;if(i>=0&&j>=0&&j<ids.length){[ids[i],ids[j]]=[ids[j],ids[i]];this.order=ids;this.changed();}}
  snapshot(){return {version:1,header:this.header,base:this.base,cuts:this.cuts,manual:this.manual,edits:[...this.edits],hidden:[...this.hidden],serial:this.serial,order:this.order};}
  restore(data){if(data.version!==1||!Array.isArray(data.base)||!Array.isArray(data.manual))throw new Error('Неверный формат проекта');const candidate=new Store();Object.assign(candidate,{header:{...candidate.header,...data.header},base:data.base,cuts:data.cuts||[],manual:data.manual,edits:new Map(data.edits||[]),hidden:new Set(data.hidden||[]),serial:number(data.serial),order:data.order||[]});const checked=candidate.rows();if(new Set(checked.map(r=>r.id)).size!==checked.length)throw new Error("Повторяющиеся ID строк");Object.assign(this,{header:candidate.header,base:candidate.base,cuts:candidate.cuts,manual:candidate.manual,edits:candidate.edits,hidden:candidate.hidden,serial:candidate.serial,order:candidate.order});this.changed();}
 }
 // Minimal standards-based XLSX writer: two worksheets, inline strings, no CDN or formulas.
 function xlsx(sheets){
  const files={};const enc=new TextEncoder();const ns='http://schemas.openxmlformats.org/spreadsheetml/2006/main';
  files['[Content_Types].xml']='<?xml version="1.0" encoding="UTF-8"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'+sheets.map((_,i)=>`<Override PartName="/xl/worksheets/sheet${i+1}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>`).join('')+'</Types>';
  files['_rels/.rels']='<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>';
  files['xl/workbook.xml']=`<workbook xmlns="${ns}" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets>`+sheets.map((s,i)=>`<sheet name="${xml(s.name)}" sheetId="${i+1}" r:id="rId${i+1}"/>`).join('')+'</sheets></workbook>';
  files['xl/_rels/workbook.xml.rels']='<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'+sheets.map((_,i)=>`<Relationship Id="rId${i+1}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet${i+1}.xml"/>`).join('')+'</Relationships>';
  const column=n=>{let s='';for(n++;n;n=Math.floor((n-1)/26))s=String.fromCharCode(65+(n-1)%26)+s;return s;};
  sheets.forEach((s,i)=>{files[`xl/worksheets/sheet${i+1}.xml`]=`<worksheet xmlns="${ns}"><sheetData>`+s.rows.map((row,r)=>`<row r="${r+1}">`+row.map((v,c)=>typeof v==='number'&&Number.isFinite(v)?`<c r="${column(c)}${r+1}"><v>${v}</v></c>`:`<c r="${column(c)}${r+1}" t="inlineStr"><is><t xml:space="preserve">${xml(v)}</t></is></c>`).join('')+'</row>').join('')+'</sheetData></worksheet>';});
  const crc=data=>{let n=0xffffffff;for(const b of data){n^=b;for(let j=0;j<8;j++)n=(n>>>1)^((n&1)?0xedb88320:0);}return (n^0xffffffff)>>>0;};
  const chunks=[],central=[];let offset=0;
  for(const [name,contents]of Object.entries(files)){
   const nameBytes=enc.encode(name),data=enc.encode(contents),checksum=crc(data);
   const head=new Uint8Array(30+nameBytes.length),v=new DataView(head.buffer);v.setUint32(0,0x04034b50,true);v.setUint16(4,20,true);v.setUint16(6,0x800,true);v.setUint32(14,checksum,true);v.setUint32(18,data.length,true);v.setUint32(22,data.length,true);v.setUint16(26,nameBytes.length,true);head.set(nameBytes,30);chunks.push(head,data);
   const dir=new Uint8Array(46+nameBytes.length),d=new DataView(dir.buffer);d.setUint32(0,0x02014b50,true);d.setUint16(4,20,true);d.setUint16(6,20,true);d.setUint16(8,0x800,true);d.setUint32(16,checksum,true);d.setUint32(20,data.length,true);d.setUint32(24,data.length,true);d.setUint16(28,nameBytes.length,true);d.setUint32(42,offset,true);dir.set(nameBytes,46);central.push(dir);offset+=head.length+data.length;
  }
  const centralSize=central.reduce((n,x)=>n+x.length,0),end=new Uint8Array(22),e=new DataView(end.buffer);e.setUint32(0,0x06054b50,true);e.setUint16(8,central.length,true);e.setUint16(10,central.length,true);e.setUint32(12,centralSize,true);e.setUint32(16,offset,true);const out=new Uint8Array(offset+centralSize+22);let pos=0;for(const chunk of [...chunks,...central,end]){out.set(chunk,pos);pos+=chunk.length;}return out;
 }
 const api={Store,number,positive,round,sourceKey,parseDxf,xlsx};root.CuttingData=api;if(typeof module!=='undefined')module.exports=api;
})(globalThis);
