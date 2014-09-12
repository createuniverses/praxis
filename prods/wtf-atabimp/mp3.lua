
mp3PausePos = 0

function pauseMp3()
    mp3PausePos = getMp3Time()
    stopMp3()
end

function unpauseMp3()
    setMp3Time(mp3PausePos)
    playMp3()
end
