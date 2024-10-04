using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.NetworkInformation;
#if !UNITY_IOS && !UNITY_WEBGL
using System.Net;
using System.Net.Sockets;
#endif
using jp.kshoji.midisystem;
using UnityEngine;
using UnityEngine.Networking;

namespace jp.kshoji.unity.midi.sample
{
    public class MidiSampleScene : MonoBehaviour, IMidiAllEventsHandler, IMidiDeviceEventHandler
    {
        private AudioSource audioSource;
        private readonly AudioClip[] audioClips = new AudioClip[128];
        readonly int[] position = new int[128];
        private readonly float[] triangleTable = new float[1024];
        private static bool isPlaySound;
#if !UNITY_IOS && !UNITY_WEBGL
        private NetworkInterface[] netInterfaces;
#endif

        private void Awake()
        {
            guiScale = (Screen.width > Screen.height) ? Screen.width / 1024f : Screen.height / 1024f;

#if !UNITY_IOS && !UNITY_WEBGL
            netInterfaces = NetworkInterface.GetAllNetworkInterfaces();
#endif

            MidiManager.Instance.RegisterEventHandleObject(gameObject);
            MidiManager.Instance.InitializeMidi(() =>
            {
#if DEVELOPMENT_BUILD
#if (UNITY_ANDROID || UNITY_IOS || UNITY_WEBGL) && !UNITY_EDITOR
                MidiManager.Instance.StartScanBluetoothMidiDevices(0);
#endif
#endif
            });

            for (var i = 0; i < 1024; i++)
            {
                if (i < 256)
                {
                    triangleTable[i] = (float)i / 256;
                }
                else if (i < 768)
                {
                    triangleTable[i] = (512f - i) / 256f;
                }
                else
                {
                    triangleTable[i] = (i - 768f) / 256f;
                }

                triangleTable[i] = Mathf.Pow(triangleTable[i], 2);
            }

            // Prepare AudioClips
            for (var i = 0; i < audioClips.Length; i++)
            {
                var note = i;
                var lengthSamples = 4410 * (14 - note / 16);
                audioClips[i] = AudioClip.Create($"note_{i}", lengthSamples, 1, 44100, false, data =>
                {
                    var sinRate = (note / 2f + 64f) / 64f;
                    var volumeRate = ((127 - note) / 2f + 64f) / 128f;
                    for (var count = 0; count < data.Length; count++)
                    {
                        data[count] = Mathf.Sin(2 * Mathf.PI * 440f * Mathf.Pow(2, (note - 69) / 12f) * position[note] / 44100) * Mathf.Pow(1f - (float)position[note] / lengthSamples, 2f) * sinRate;
                        data[count] +=
                            triangleTable[
                                (int)(1024 * 440f * Mathf.Pow(2, (note - 69) / 12f) * position[note] / 44100) % 1024] * Mathf.Pow(1f - (float)position[note] / lengthSamples, 2f) *
                            (2f - sinRate);
                        data[count] /= 2f;
                        data[count] *= volumeRate;
                        position[note] += 1;
                    }
                }, newPosition =>
                {
                    position[note] = newPosition;
                });
            }

            audioSource = GetComponent<AudioSource>();
            MidiSystem.AddReceiver("MidiNoteReceiver", new MidiNoteReceiver());
        }

        /// <summary>
        /// Receiver for playing AudioClips
        /// </summary>
        private class MidiNoteReceiver : IReceiver
        {
            /// <inheritdoc cref="IReceiver.Send"/>
            public void Send(MidiMessage message, long timeStamp)
            {
                if (message is ShortMessage shortMessage)
                {
                    switch (shortMessage.GetStatus() & ShortMessage.MaskEvent)
                    {
                        case ShortMessage.NoteOn:
                            if (isPlaySound)
                            {
                                lock (NoteOnQueue)
                                {
                                    NoteOnQueue.Enqueue(shortMessage);
                                }
                            }
                            break;
                    }
                }
            }

            /// <inheritdoc cref="IReceiver.Close"/>
            public void Close()
            {
            }
        }

        private static readonly Queue<ShortMessage> NoteOnQueue = new Queue<ShortMessage>();
        private void Update()
        {
            // process playing AudioClips
            lock (NoteOnQueue)
            {
                while (NoteOnQueue.Count > 0)
                {
                    var note = NoteOnQueue.Dequeue();
                    audioSource.PlayOneShot(audioClips[note.GetMessage()[1]], note.GetMessage()[2] / 127f);
                }
            }
        }

        private const int MaxNumberOfReceiverMidiMessages = 50;
        private readonly List<string> receivedMidiMessages = new List<string>();

        private const int SendMidiWindow = 0;
        private const int ReceiveMidiWindow = 1;
        private const int MidiPlayerWindow = 2;
        private const int MidiConnectionWindow = 3;

        private Rect midiConnectionWindowRect = new Rect(0, 0, 400, 400);
        private Rect sendMidiWindowRect = new Rect(25, 25, 400, 400);
        private Rect receiveMidiWindowRect = new Rect(50, 50, 400, 400);
        private Rect midiPlayerWindowRect = new Rect(75, 75, 400, 400);

        private int outputDeviceIdIndex;
        private string selectedOutputDeviceId;
        private int inputDeviceIdIndex;
        private string selectedInputDeviceId;
        private float channel;
        private float noteNumber = 64f;
        private float velocity = 100f;
        private float program;
        private float controlFunction;
        private float controlValue;
        private Vector2 receiveMidiWindowScrollPosition;
        private string sampleSmfUrl = "https://bitmidi.com/uploads/14947.mid";
        private SequencerImpl sequencer;
        private bool isSequencerOpened;
        private float guiScale;
        private bool mpeEnabled;

#if !UNITY_IOS && !UNITY_WEBGL
        private string ipAddress = "192.168.0.100";
        private string port = "5004";
#endif

        private bool isBluetoothScanning;
        private bool isBluetoothAdvertising;
        private bool isNearbyDiscovering;
        private bool isNearbyAdvertising;

        private void OnGUI()
        {
            if (Event.current.type != EventType.Layout)
            {
                return;
            }

            GUIUtility.ScaleAroundPivot(new Vector2(guiScale, guiScale), Vector2.zero);

            sendMidiWindowRect = GUILayout.Window(SendMidiWindow, sendMidiWindowRect, OnGUIWindow, "Send MIDI");
            receiveMidiWindowRect = GUILayout.Window(ReceiveMidiWindow, receiveMidiWindowRect, OnGUIWindow, "Receive MIDI");
            midiPlayerWindowRect = GUILayout.Window(MidiPlayerWindow, midiPlayerWindowRect, OnGUIWindow, "MIDI Player");

            // MidiConnectionWindow may be empty under some platforms, See OnGUIWindow implementation
            var isMidiConnectionWindowEnabled = false;
#if ((UNITY_IOS || UNITY_ANDROID || UNITY_WEBGL) && !UNITY_EDITOR)
            isMidiConnectionWindowEnabled = true;
#endif
#if UNITY_ANDROID && !UNITY_EDITOR
            isMidiConnectionWindowEnabled = true;
#endif
#if (UNITY_STANDALONE_OSX || UNITY_EDITOR_OSX || ((UNITY_IOS || UNITY_ANDROID) && !UNITY_EDITOR)) && ENABLE_NEARBY_CONNECTIONS
            isMidiConnectionWindowEnabled = true;
#endif
#if !UNITY_IOS && !UNITY_WEBGL
            isMidiConnectionWindowEnabled = true;
#endif
            if (isMidiConnectionWindowEnabled)
            {
                midiConnectionWindowRect = GUILayout.Window(MidiConnectionWindow, midiConnectionWindowRect, OnGUIWindow, "MIDI Connections");
            }
        }

        void UpdateReceiverConnections()
        {
            var receivers = new HashSet<IReceiver>();
            if (selectedOutputDeviceId != null)
            {
                var receiver = MidiSystem.GetReceiver(selectedOutputDeviceId);
                if (receiver != null)
                {
                    receivers.Add(receiver);
                }
            }

            var midiNoteReceiver = MidiSystem.GetReceiver("MidiNoteReceiver");
            if (midiNoteReceiver != null)
            {
                receivers.Add(midiNoteReceiver);
            }

            sequencer.UpdateDeviceConnections(receivers, new HashSet<ITransmitter>());
        }

        private void OnGUIWindow(int id)
        {
            var outputDeviceIds = MidiManager.Instance.OutputDeviceIdSet.ToArray();
            var inputDeviceIds = MidiManager.Instance.InputDeviceIdSet.ToArray();
            switch (id)
            {
                case SendMidiWindow:
                    GUILayout.Label("Device to send MIDI: ");
                    if (outputDeviceIds.Length == 0)
                    {
                        GUILayout.Label("No output devices connected");
                    }
                    else
                    {
                        // get device name for device ID
                        var deviceNames = new string[outputDeviceIds.Length];
                        for (var i = 0; i < outputDeviceIds.Length; i++)
                        {
                            deviceNames[i] = $"{MidiManager.Instance.GetDeviceName(outputDeviceIds[i])} ({outputDeviceIds[i]})";
                        }
                        outputDeviceIdIndex = GUILayout.SelectionGrid(outputDeviceIdIndex, deviceNames, 1);
                        if (outputDeviceIdIndex >= outputDeviceIds.Length)
                        {
                            outputDeviceIdIndex = outputDeviceIds.Length - 1;
                        }
                        selectedOutputDeviceId = outputDeviceIds[outputDeviceIdIndex];

                        GUILayout.Label("Setup MPE feature on selected device");
                        if (!mpeEnabled)
                        {
                            if (GUILayout.Button("Enable MPE"))
                            {
                                MpeManager.Instance.SetupMpeZone(selectedOutputDeviceId, 0, 15);
                                mpeEnabled = true;
                            }
                        }
                        else
                        {
                            if (GUILayout.Button("Disable MPE"))
                            {
                                MpeManager.Instance.SetupMpeZone(selectedOutputDeviceId, 0, 0);
                                mpeEnabled = false;
                            }
                        }

                        GUILayout.Label($"Channel: {(int)channel}");
                        channel = GUILayout.HorizontalSlider(channel, 0, 16.9f);
                        GUILayout.Label($"Note: {(int)noteNumber}");
                        noteNumber = GUILayout.HorizontalSlider(noteNumber, 0, 127.9f);
                        GUILayout.Label($"Velocity: {(int)velocity}");
                        velocity = GUILayout.HorizontalSlider(velocity, 0, 127.9f);

                        if (GUILayout.Button("NoteOn"))
                        {
                            if (mpeEnabled)
                            {
                                MpeManager.Instance.SendMpeNoteOn(selectedOutputDeviceId, 0, (int)noteNumber, (int)velocity);
                            }
                            else
                            {
                                MidiManager.Instance.SendMidiNoteOn(selectedOutputDeviceId, 0, (int)channel, (int)noteNumber, (int)velocity);
                            }
                        }
                        if (GUILayout.Button("NoteOff"))
                        {
                            if (mpeEnabled)
                            {
                                MpeManager.Instance.SendMpeNoteOff(selectedOutputDeviceId, 0, (int)noteNumber, (int)velocity);
                            }
                            else
                            {
                                MidiManager.Instance.SendMidiNoteOff(selectedOutputDeviceId, 0, (int)channel, (int)noteNumber, (int)velocity);
                            }
                        }
                    
                        GUILayout.Label($"Program: {(int)program}");
                        program = GUILayout.HorizontalSlider(program, 0, 127.9f);
                        if (GUILayout.Button("ProgramChange"))
                        {
                            if (mpeEnabled)
                            {
                                MpeManager.Instance.SendMpeProgramChange(selectedOutputDeviceId, 0, (int)program);
                            }
                            else
                            {
                                MidiManager.Instance.SendMidiProgramChange(selectedOutputDeviceId, 0, (int)channel, (int)program);
                            }
                        }

                        GUILayout.Label($"Control Function: {(int)controlFunction}");
                        controlFunction = GUILayout.HorizontalSlider(controlFunction, 0, 127.9f);
                        GUILayout.Label($"Control Value: {(int)controlValue}");
                        controlValue = GUILayout.HorizontalSlider(controlValue, 0, 127.9f);
                        if (GUILayout.Button("ControlChange"))
                        {
                            if (mpeEnabled)
                            {
                                MpeManager.Instance.SendMpeControlChange(selectedOutputDeviceId, 0, (int)controlFunction, (int)controlValue);
                            }
                            else
                            {
                                MidiManager.Instance.SendMidiControlChange(selectedOutputDeviceId, 0, (int)channel, (int)controlFunction, (int)controlValue);
                            }
                        }
                    }
                    break;

                case ReceiveMidiWindow:
                    GUILayout.Label("Device to receive MIDI: ");
                    if (inputDeviceIds.Length == 0)
                    {
                        GUILayout.Label("No input devices connected");
                    }
                    else
                    {
                        // get device name for device ID
                        var deviceNames = new string[inputDeviceIds.Length];
                        for (var i = 0; i < inputDeviceIds.Length; i++)
                        {
                            deviceNames[i] = $"{MidiManager.Instance.GetDeviceName(inputDeviceIds[i])} ({inputDeviceIds[i]})";
                        }
                        inputDeviceIdIndex = GUILayout.SelectionGrid(inputDeviceIdIndex, deviceNames, 1);
                        if (inputDeviceIdIndex >= inputDeviceIds.Length)
                        {
                            inputDeviceIdIndex = inputDeviceIds.Length - 1;
                        }
                        selectedInputDeviceId = inputDeviceIds[inputDeviceIdIndex];
                    }

                    receiveMidiWindowScrollPosition = GUILayout.BeginScrollView(receiveMidiWindowScrollPosition);
                    GUILayout.Label("Midi messages: ");
                    if (receivedMidiMessages.Count > MaxNumberOfReceiverMidiMessages)
                    {
                        receivedMidiMessages.RemoveRange(0, receivedMidiMessages.Count - MaxNumberOfReceiverMidiMessages);
                    }
                    foreach (var message in receivedMidiMessages.AsReadOnly().Reverse())
                    {
                        GUILayout.Label(message);
                    }
                    GUILayout.EndScrollView();
                    break;

                case MidiPlayerWindow:
                    if (sequencer == null)
                    {
                        isSequencerOpened = false;
                        sequencer = new SequencerImpl(() => { isSequencerOpened = true; });
#if UNITY_WEBGL || !UNITY_5_3_OR_NEWER
                        StartCoroutine(sequencer.OpenCoroutine());
#else
                        sequencer.Open();
#endif

                        // logging sequencer events
                        sequencer.OnSequenceFinished += () =>
                        {
                            Screen.sleepTimeout = SleepTimeout.SystemSetting;
                            receivedMidiMessages.Add("SMF playback finished.");
                        };
                        sequencer.OnSequenceStarted += () =>
                        {
                            Screen.sleepTimeout = SleepTimeout.NeverSleep;
                            receivedMidiMessages.Add("SMF playback started.");
                        };
                        sequencer.OnSequencePaused += () =>
                        {
                            Screen.sleepTimeout = SleepTimeout.SystemSetting;
                            receivedMidiMessages.Add("SMF playback paused.");
                        };
                        sequencer.OnSequenceTimeUpdated += (time) =>
                        {
                            // update seekbar, displaying time etc.
                        };
                    }

                    GUILayout.Label("Play SMF");

                    isPlaySound = GUILayout.Toggle(isPlaySound, "Play sound on Device");

                    GUILayout.Label("Sample SMF URL:");
                    sampleSmfUrl = GUILayout.TextField(sampleSmfUrl);
                    if (GUILayout.Button("Play sample SMF") && isSequencerOpened)
                    {
                        // load SMF from url
                        // Original source is: http://www.piano-midi.de/midis/bach/bach_847.mid
                        // Have specified another site because it need to access with `https'.
                        StartCoroutine(GetStreamFromUrl(sampleSmfUrl, stream =>
                        {
                            UpdateReceiverConnections();

                            sequencer.Stop();
                            sequencer.SetSequence(stream);
                            sequencer.Start();
                        }));

#if false
                        // load SMF from addressable assets
                        // TODO: install 'Addressable Assets System' package (com.unity.addressables) from Unity Package Manager
                        // TODO: store SMF to Assets/MIDI/Samples/Data/sample.mid.bytes
                        // TODO: with Inspector, mark asset 'Assets/MIDI/Samples/Data/sample.mid.bytes' as Addressable
                        UnityEngine.AddressableAssets.Addressables.LoadAssetAsync<TextAsset>("Assets/MIDI/Samples/Data/sample.mid.bytes").Completed += obj =>
                        {
                            UpdateReceiverConnections();

                            sequencer.Stop();
                            sequencer.SetSequence(new MemoryStream(obj.Result.bytes));
                            sequencer.Start();
                        };
#endif

#if false
                        // load SMF from streaming assets
                        // TODO: store a SMF into 'Assets/StreamingAssets/sample.mid'
                        StartCoroutine(GetStreamingAssetFilePath("sample.mid", stream =>
                        {
                            UpdateReceiverConnections();

                            sequencer.Stop();
                            sequencer.SetSequence(stream);
                            sequencer.Start();
                        }));
#endif
                    }

                    if (GUILayout.Button("Pause playing") && isSequencerOpened)
                    {
                        sequencer.Stop();
                    }

                    if (GUILayout.Button("Resume playing") && isSequencerOpened)
                    {
                        sequencer.Start();
                    }

                    if (GUILayout.Button("Stop playing") && isSequencerOpened)
                    {
                        sequencer.Stop();
                        sequencer.SetTickPosition(0);
                    }

                    GUILayout.Label("Record SMF");

                    if (GUILayout.Button("Start recording SMF") && isSequencerOpened)
                    {
                        var transmitters = new HashSet<ITransmitter>();
                        if (selectedInputDeviceId != null)
                        {
                            var transmitter = MidiSystem.GetTransmitter(selectedOutputDeviceId);
                            if (transmitter != null)
                            {
                                transmitters.Add(transmitter);
                            }
                        }
                        sequencer.UpdateDeviceConnections(new HashSet<IReceiver>(), transmitters);

                        sequencer.SetSequence(new Sequence(Sequence.Ppq, 480));
                        if (sequencer.GetIsRecording())
                        {
                            sequencer.StopRecording();
                        }
                        sequencer.StartRecording();
                    }

                    var recordedSmfPath = Path.Combine(Application.persistentDataPath, "record.mid");
                    if (GUILayout.Button("Stop recording") && isSequencerOpened)
                    {
                        sequencer.Stop();

                        var sequence = sequencer.GetSequence();
                        if (sequence.GetTickLength() > 0)
                        {
                            using (var stream = new FileStream(recordedSmfPath, FileMode.Create, FileAccess.Write))
                            {
                                MidiSystem.WriteSequence(sequence, stream);
                            }
                        }
                    }

                    if (File.Exists(recordedSmfPath) && GUILayout.Button("Play recorded SMF") && isSequencerOpened)
                    {
                        using (var stream = new FileStream(recordedSmfPath, FileMode.Open, FileAccess.Read))
                        {
                            UpdateReceiverConnections();

                            sequencer.Stop();
                            sequencer.SetSequence(stream);
                            sequencer.Start();
                        }
                    }

                    if (File.Exists(recordedSmfPath) && GUILayout.Button("Stop playing") && isSequencerOpened)
                    {
                        sequencer.Stop();
                        sequencer.SetTickPosition(0);
                    }

                    break;

                case MidiConnectionWindow:
#if (UNITY_IOS || UNITY_ANDROID || UNITY_WEBGL) && !UNITY_EDITOR
                    GUILayout.Label("BLE MIDI Central:");
                    if (isBluetoothScanning)
                    {
                        if (GUILayout.Button("Stop scan BLE MIDI devices"))
                        {
                            MidiManager.Instance.StopScanBluetoothMidiDevices();
                            isBluetoothScanning = false;
                        }
                    }
                    else
                    {
                        if (GUILayout.Button("Scan BLE MIDI devices"))
                        {
                            MidiManager.Instance.StartScanBluetoothMidiDevices(0);
                            isBluetoothScanning = true;
                        }
                    }
                    GUILayout.Space(20f);
#endif

#if UNITY_ANDROID && !UNITY_EDITOR
                    GUILayout.Label("BLE MIDI Peripheral:");
                    if (isBluetoothAdvertising)
                    {
                        if (GUILayout.Button("Stop act as BLE MIDI device"))
                        {
                            MidiManager.Instance.StopAdvertisingBluetoothMidiDevice();
                            isBluetoothAdvertising = false;
                        }
                    }
                    else
                    {
                        if (GUILayout.Button("Act as BLE MIDI device"))
                        {
                            MidiManager.Instance.StartAdvertisingBluetoothMidiDevice();
                            isBluetoothAdvertising = true;
                        }
                    }
                    GUILayout.Space(20f);
#endif

                    // NOTE: To use Nearby Connections MIDI feature, follow these processes below.
                    // Add a package with Unity Package Manager.
                    // Select `Add package from git URL...` and input this URL.
                    // ssh://git@github.com/kshoji/Nearby-Connections-for-Unity.git
                    // or this URL
                    // git+https://github.com/kshoji/Nearby-Connections-for-Unity
                    //
                    // Add a Scripting Define Symbol to Player settings
                    // ENABLE_NEARBY_CONNECTIONS
#if (UNITY_STANDALONE_OSX || UNITY_EDITOR_OSX || ((UNITY_IOS || UNITY_ANDROID) && !UNITY_EDITOR)) && ENABLE_NEARBY_CONNECTIONS
                    GUILayout.Label("Nearby MIDI:");
                    if (isNearbyDiscovering)
                    {
                        if (GUILayout.Button("Stop discover Nearby MIDI devices"))
                        {
                            MidiManager.Instance.StopNearbyDiscovering();
                            isNearbyDiscovering = false;
                        }
                    }
                    else
                    {
                        if (GUILayout.Button("Discover Nearby MIDI devices"))
                        {
                            MidiManager.Instance.StartNearbyDiscovering();
                            isNearbyDiscovering = true;
                        }
                    }
                    GUILayout.Space(20f);
                    if (isNearbyAdvertising)
                    {
                        if (GUILayout.Button("Stop advertise Nearby MIDI devices"))
                        {
                            MidiManager.Instance.StopNearbyAdvertising();
                            isNearbyAdvertising = false;
                        }
                    }
                    else
                    {
                        if (GUILayout.Button("Advertise Nearby MIDI devices"))
                        {
                            MidiManager.Instance.StartNearbyAdvertising();
                            isNearbyAdvertising = true;
                        }
                    }
                    GUILayout.Space(20f);
#endif

#if !UNITY_IOS && !UNITY_WEBGL
                    foreach (var netInterface in netInterfaces)
                    {
                        var properties = netInterface.GetIPProperties();
                        foreach (var unicast in properties.UnicastAddresses)
                        {
                            var address = unicast.Address;
                            if (address.IsIPv6LinkLocal || address.IsIPv6Multicast || address.IsIPv6SiteLocal || address.IsIPv4MappedToIPv6 || address.IsIPv6Teredo)
                            {
                                continue;
                            }
                            if (address.AddressFamily != AddressFamily.InterNetwork && address.AddressFamily != AddressFamily.InterNetworkV6)
                            {
                                continue;
                            }
                            if (IPAddress.IsLoopback(address))
                            {
                                continue;
                            }
                            GUILayout.BeginHorizontal();
                            GUILayout.Label(address.ToString());
                            if (GUILayout.Button("Copy to clipboard"))
                            {
                                GUIUtility.systemCopyBuffer = address.ToString();
                            }
                            GUILayout.EndHorizontal();
                        }
                    }

                    if (MidiManager.Instance.IsRtpMidiRunning(5004))
                    {
                        if (GUILayout.Button("Stop RTP MIDI"))
                        {
                            MidiManager.Instance.StopRtpMidi(5004);
                        }
                    }
                    else
                    {
                        if (GUILayout.Button("Start RTP MIDI Server"))
                        {
                            MidiManager.Instance.StartRtpMidiServer("RtpMidiSession", 5004);
                        }

                        GUILayout.Space(20f);

                        GUILayout.Label("RTP MIDI destination:");
                        ipAddress = GUILayout.TextField(ipAddress);
                        port = GUILayout.TextField(port);
                        if (GUILayout.Button("Connect to RTP MIDI Server"))
                        {
                            MidiManager.Instance.ConnectToRtpMidiServer("RtpMidiSession", 5004, new IPEndPoint(IPAddress.Parse(ipAddress), int.Parse(port)));
                        }
                    }
#endif
                    break;
            }
            GUI.DragWindow();
        }

        private bool isPaused = false;
        private void OnApplicationPause(bool pauseStatus)
        {
            if (pauseStatus)
            {
                if (sequencer != null && sequencer.GetIsRunning())
                {
                    sequencer?.Stop();
                    isPaused = true;
                }
            }
            else
            {
                if (isPaused)
                {
                    sequencer?.Start();
                    isPaused = false;
                }
            }
        }

        private void OnApplicationQuit()
        {
            MidiManager.Instance.TerminateMidi();
        }

        /// <summary>
        /// Get <see cref="Stream"/> from url
        /// </summary>
        /// <param name="url"></param>
        /// <param name="onResult"></param>
        /// <returns></returns>
        IEnumerator GetStreamFromUrl(string url, Action<Stream> onResult)
        {
            using (var www = UnityWebRequest.Get(url))
            {
                yield return www.SendWebRequest();
                onResult(new MemoryStream(www.downloadHandler.data));
            }
        }

        /// <summary>
        /// Get <see cref="Stream"/> for a Streaming Asset
        /// </summary>
        /// <param name="filename">asset file name</param>
        /// <param name="onResult">Action for getting <see cref="Stream"/></param>
        /// <returns></returns>
        IEnumerator GetStreamingAssetFilePath(string filename, Action<Stream> onResult)
        {
#if UNITY_ANDROID || UNITY_WEBGL
            var path = Path.Combine(Application.streamingAssetsPath, filename);
            if (path.Contains("://"))
            {
                using (var www = UnityWebRequest.Get(path))
                {
                    yield return www.SendWebRequest();
                    onResult(new MemoryStream(www.downloadHandler.data));
                }
            }
            else
            {
                onResult(new FileStream(path, FileMode.Open, FileAccess.Read));
            }
#else
            onResult(new FileStream(Path.Combine(Application.streamingAssetsPath, filename), FileMode.Open, FileAccess.Read));
            yield break;
#endif
        }

        private void OnDestroy()
        {
            sequencer?.Close();
        }

        public void OnMidiNoteOn(string deviceId, int group, int channel, int note, int velocity)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiNoteOn from: {deviceId}, channel: {channel}, note: {note}, velocity: {velocity}");
            }

            if (isPlaySound)
            {
                lock (NoteOnQueue)
                {
                    NoteOnQueue.Enqueue(new ShortMessage(ShortMessage.NoteOn, channel, note, velocity));
                }
            }
        }

        public void OnMidiNoteOff(string deviceId, int group, int channel, int note, int velocity)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiNoteOff from: {deviceId}, channel: {channel}, note: {note}, velocity: {velocity}");
            }
        }

        public void OnMidiContinue(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiContinue from: {deviceId}");
            }
        }

        public void OnMidiReset(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiReset from: {deviceId}");
            }
        }

        public void OnMidiStart(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiStart from: {deviceId}");
            }
        }

        public void OnMidiStop(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiStop from: {deviceId}");
            }
        }

        public void OnMidiActiveSensing(string deviceId, int group)
        {
            // too many events received, so commented out
            // receivedMidiMessages.Add("OnMidiActiveSensing");
        }

        public void OnMidiCableEvents(string deviceId, int group, int byte1, int byte2, int byte3)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiCableEvents from: {deviceId}, byte1: {byte1}, byte2: {byte2}, byte3: {byte3}");
            }
        }

        public void OnMidiChannelAftertouch(string deviceId, int group, int channel, int pressure)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiChannelAftertouch from: {deviceId}, channel: {channel}, pressure: {pressure}");
            }
        }

        public void OnMidiPitchWheel(string deviceId, int group, int channel, int amount)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiPitchWheel from: {deviceId}, channel: {channel}, amount: {amount}");
            }
        }

        public void OnMidiPolyphonicAftertouch(string deviceId, int group, int channel, int note, int pressure)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiPolyphonicAftertouch from: {deviceId}, channel: {channel}, note: {note}, pressure: {pressure}");
            }
        }

        public void OnMidiProgramChange(string deviceId, int group, int channel, int program)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiProgramChange from: {deviceId}, channel: {channel}, program: {program}");
            }
        }

        public void OnMidiControlChange(string deviceId, int group, int channel, int function, int value)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiControlChange from: {deviceId}, channel: {channel}, function: {function}, value: {value}");
            }
        }

        public void OnMidiSongSelect(string deviceId, int group, int song)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiSongSelect from: {deviceId}, song: {song}");
            }
        }

        public void OnMidiSongPositionPointer(string deviceId, int group, int position)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiSongPositionPointer from: {deviceId}, song: {position}");
            }
        }

        public void OnMidiSingleByte(string deviceId, int group, int byte1)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiSingleByte from: {deviceId}, byte1: {byte1}");
            }
        }

        public void OnMidiSystemExclusive(string deviceId, int group, byte[] systemExclusive)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiSystemExclusive from: {deviceId}, systemExclusive: {BitConverter.ToString(systemExclusive).Replace("-", " ")}");
            }
        }

        public void OnMidiSystemCommonMessage(string deviceId, int group, byte[] message)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiSystemCommonMessage from: {deviceId}, message: {BitConverter.ToString(message).Replace("-", " ")}");
            }
        }

        public void OnMidiTimeCodeQuarterFrame(string deviceId, int group, int timing)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiTimeCodeQuarterFrame from: {deviceId}, timing: {timing}");
            }
        }

        public void OnMidiTimingClock(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiTimingClock from: {deviceId}");
            }
        }

        public void OnMidiTuneRequest(string deviceId, int group)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiTuneRequest from: {deviceId}");
            }
        }

        public void OnMidiMiscellaneousFunctionCodes(string deviceId, int group, int byte1, int byte2, int byte3)
        {
            if (selectedInputDeviceId == deviceId)
            {
                receivedMidiMessages.Add($"OnMidiMiscellaneousFunctionCodes from: {deviceId}, byte1: {byte1}, byte2: {byte2}, byte3: {byte3}");
            }
        }
        
        public void OnMidiInputDeviceAttached(string deviceId)
        {
            receivedMidiMessages.Add($"MIDI Input device attached. deviceId: {deviceId}, name: {MidiManager.Instance.GetDeviceName(deviceId)}, vendor: {MidiManager.Instance.GetVendorId(deviceId)}, product: {MidiManager.Instance.GetProductId(deviceId)}");
        }

        public void OnMidiOutputDeviceAttached(string deviceId)
        {
            receivedMidiMessages.Add($"MIDI Output device attached. deviceId: {deviceId}, name: {MidiManager.Instance.GetDeviceName(deviceId)}, vendor: {MidiManager.Instance.GetVendorId(deviceId)}, product: {MidiManager.Instance.GetProductId(deviceId)}");
        }

        public void OnMidiInputDeviceDetached(string deviceId)
        {
            receivedMidiMessages.Add($"MIDI Input device detached. deviceId: {deviceId}");
        }

        public void OnMidiOutputDeviceDetached(string deviceId)
        {
            receivedMidiMessages.Add($"MIDI Output device detached. deviceId: {deviceId}");
        }
    }
}