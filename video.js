import Vimeo from '@vimeo/player'
makeIntroVideoPlay = () => {
    var iframe = document.querySelector('.intro-player');
    var player = new Vimeo.Player(iframe);
    player.play();

    player.on('play', function () {
        console.log('Played the video');
    });

    player.getVideoTitle().then(function (title) {
        console.log('title:', title);
    });
}
setTimeout(makeIntroVideoPlay, 800);