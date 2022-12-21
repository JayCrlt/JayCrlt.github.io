$(document).ready(function () {
    $('.gallery-item.item-4x3:lt(10)').show();
    $('.less').hide();
    var items =  50;
    var shown =  10;
    $('.more').click(function () {
        $('.less').show();
        shown = $('.gallery-item.item-4x3:visible').length+20;
        if(shown< items) {
          $('.gallery-item.item-4x3:lt('+shown+')').show(300);
        } else {
          $('.gallery-item.item-4x3:lt('+items+')').show(300);
          $('.more').hide();
        }
    });
    $('.less').click(function () {
        $('.gallery-item.item-4x3').not(':lt(10)').hide(300);
        $('.more').show();
        $('.less').hide();
    });
});