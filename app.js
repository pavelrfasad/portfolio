const toast = document.querySelector('.toast');
const email = 'pavelr.fasad@gmail.com';
let timer;
async function copyEmail() {
 let copied = false;
 try {
  await navigator.clipboard.writeText(email);
  copied = true;
 } catch {
  const field = document.createElement('textarea');
  field.value = email;
  field.setAttribute('readonly', '');
  field.style.cssText = 'position:fixed;left:-9999px;top:0';
  document.body.appendChild(field);
  field.select();
  try { copied = document.execCommand('copy'); } catch {}
  field.remove();
 }
 toast.textContent = copied ? 'Email скопирован: ' + email : 'Скопируйте адрес: ' + email;
 toast.classList.add('visible');
 clearTimeout(timer);
 timer = setTimeout(() => toast.classList.remove('visible'), 4500);
}
document.querySelectorAll('[data-copy-email]').forEach(button => {
 button.addEventListener('click', copyEmail);
});
document.querySelector('#year').textContent = new Date().getFullYear();
