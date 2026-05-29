function setupRadioDisabling() {
    function handleRadioLogic() {
        const radios = document.querySelectorAll('input[name="Enrichment_source"]');
        const selectedRadio = document.querySelector('input[name="Enrichment_source"]:checked');
        
        // 重置所有選項狀態
        radios.forEach(r => {
            r.disabled = false;
            r.parentElement.classList.remove('disabled', 'selected');
        });
        
        // 檢查是否選中 DE
        if (selectedRadio && selectedRadio.value === 'DE') {
            radios.forEach(r => {
                if (r.value !== 'DE') {
                    r.disabled = true;
                    r.parentElement.classList.add('disabled');
                } else {
                    r.parentElement.classList.add('selected');
                }
            });
        }
    }
    
    // 使用事件委派監聽用戶點擊
    document.addEventListener('change', function(e) {
        if (e.target.name === 'Enrichment_source') {
            handleRadioLogic();
        }
    });
    
    // 將函數暴露到全域，供 R 調用
    window.checkRadioStatus = handleRadioLogic;
}

document.addEventListener('DOMContentLoaded', setupRadioDisabling);