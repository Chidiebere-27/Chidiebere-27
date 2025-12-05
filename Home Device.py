import tkinter as tk
from tkinter import messagebox, simpledialog
import csv
import os
from typing import Dict, Any

# Base device class remains the same
class SmartDevice:
    def __init__(self):
        self._switched_on = False

    def toggle_switch(self):
        self._switched_on = not self._switched_on

    @property
    def switched_on(self):
        return self._switched_on

    @switched_on.setter
    def switched_on(self, value: bool):
        if isinstance(value, bool):
            self._switched_on = value
        else:
            raise ValueError("Switch state must be boolean")

    def to_dict(self) -> Dict[str, Any]:
        return {
            'type': self.__class__.__name__,
            'switched_on': self.switched_on
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'SmartDevice':
        device_type = data['type']
        if device_type == 'SmartPlug':
            return SmartPlug._from_dict(data)
        elif device_type == 'SmartHeater':
            return SmartHeater._from_dict(data)
        elif device_type == 'SmartDoor':
            return SmartDoor._from_dict(data)
        else:
            raise ValueError(f"Unknown device type: {device_type}")

    def __str__(self):
        state = "on" if self.switched_on else "off"
        return f"{self.__class__.__name__} is {state}"

class SmartPlug(SmartDevice):
    def __init__(self, consumption_rate: int):
        super().__init__()
        self._consumption_rate = None
        self.consumption_rate = consumption_rate

    @property
    def consumption_rate(self) -> int:
        return self._consumption_rate

    @consumption_rate.setter
    def consumption_rate(self, value: int):
        if 0 <= value <= 150:
            self._consumption_rate = value
        else:
            raise ValueError("Consumption rate must be between 0-150W")

    def to_dict(self) -> Dict[str, Any]:
        data = super().to_dict()
        data.update({
            'consumption_rate': self.consumption_rate
        })
        return data

    @classmethod
    def _from_dict(cls, data: Dict[str, Any]) -> 'SmartPlug':
        return cls(data['consumption_rate'])

    def __str__(self):
        base_str = super().__str__()
        return f"{base_str} with consumption rate {self.consumption_rate}W"

class SmartHeater(SmartDevice):
    def __init__(self):
        super().__init__()
        self._setting = 2

    @property
    def setting(self) -> int:
        return self._setting

    @setting.setter
    def setting(self, value: int):
        if 0 <= value <= 5 and isinstance(value, int):
            self._setting = value
        else:
            raise ValueError("Setting must be an integer between 0-5")

    def to_dict(self) -> Dict[str, Any]:
        data = super().to_dict()
        data.update({
            'setting': self.setting
        })
        return data

    @classmethod
    def _from_dict(cls, data: Dict[str, Any]) -> 'SmartHeater':
        heater = cls()
        heater.setting = data['setting']
        return heater

    def __str__(self):
        base_str = super().__str__()
        return f"{base_str} with setting {self.setting}"

class SmartDoor(SmartDevice):
    def __init__(self):
        super().__init__()
        self._locked = True

    @property
    def locked(self) -> bool:
        return self._locked

    @locked.setter
    def locked(self, value: bool):
        if isinstance(value, bool):
            self._locked = value
        else:
            raise ValueError("Locked must be a boolean value")

    def to_dict(self) -> Dict[str, Any]:
        data = super().to_dict()
        data.update({
            'locked': self.locked
        })
        return data

    @classmethod
    def _from_dict(cls, data: Dict[str, Any]) -> 'SmartDoor':
        door = cls()
        door.locked = data['locked']
        return door

    def __str__(self):
        base_str = super().__str__()
        return f"{base_str} with locked {self.locked}"

class SmartHome:
    MAX_DEVICES = 10

    def __init__(self, name: str):
        self.name = name
        self.devices = []

    def add_device(self, device: SmartDevice):
        if len(self.devices) >= self.MAX_DEVICES:
            raise Exception("Device limit reached")
        self.devices.append(device)

    def remove_device(self, index: int):
        if 0 <= index < len(self.devices):
            del self.devices[index]
        else:
            raise IndexError("Invalid device index")

    def toggle_device(self, index: int):
        if 0 <= index < len(self.devices):
            self.devices[index].toggle_switch()
        else:
            raise IndexError("Invalid device index")

    def switch_all(self, state: bool):
        for device in self.devices:
            device.switched_on = state

    def update_option(self, index: int, **kwargs):
        if 0 <= index < len(self.devices):
            device = self.devices[index]
            for key, value in kwargs.items():
                if hasattr(device, key):
                    setattr(device, key, value)
                else:
                    raise AttributeError(f"{device.__class__.__name__} has no attribute {key}")
        else:
            raise IndexError("Invalid device index")

    def get_status_summary(self) -> str:
        total = len(self.devices)
        active = sum(1 for d in self.devices if d.switched_on)
        return f"{self.name} ({active}/{total} active)"

    def to_dict(self) -> Dict[str, Any]:
        return {
            'name': self.name,
            'devices': [d.to_dict() for d in self.devices]
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'SmartHome':
        home = cls(data['name'])
        for device_data in data['devices']:
            home.add_device(SmartDevice.from_dict(device_data))
        return home

class SmartHomesApp:
    STORAGE_FILE = 'smart_homes.csv'

    def __init__(self):
        self.homes = {}
        self.load_from_csv()

    def add_home(self, name: str):
        if name in self.homes:
            raise ValueError("Home already exists")
        self.homes[name] = SmartHome(name)

    def remove_home(self, name: str):
        if name in self.homes:
            del self.homes[name]

    def get_home(self, name: str) -> SmartHome:
        return self.homes.get(name)

    def get_all_homes(self) -> Dict[str, SmartHome]:
        return self.homes

    def save_to_csv(self):
        with open(self.STORAGE_FILE, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['Home Name', 'Devices'])
            for home in self.homes.values():
                devices = [d.to_dict() for d in home.devices]
                writer.writerow([home.name, devices])

    def load_from_csv(self):
        if not os.path.exists(self.STORAGE_FILE):
            return

        with open(self.STORAGE_FILE, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                name = row['Home Name']
                devices_data = eval(row['Devices'])
                home = SmartHome(name)
                for device_data in devices_data:
                    home.add_device(SmartDevice.from_dict(device_data))
                self.homes[name] = home

class SmartHomeManagerApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Smart Homes Manager")
        self.smart_homes_app = SmartHomesApp()
        self.current_home = None
        self.current_home_window = None

        # Main frame
        self.frame = tk.Frame(root)
        self.frame.pack(padx=10, pady=10)

        # Homes list
        self.home_list = tk.Listbox(self.frame, width=50)
        self.home_list.grid(row=0, column=0, columnspan=3, padx=5, pady=5)
        self.update_home_list()

        # Buttons
        self.add_btn = tk.Button(self.frame, text="Add Home", command=self.add_home)
        self.add_btn.grid(row=1, column=0, padx=5, pady=5)

        self.remove_btn = tk.Button(self.frame, text="Remove Home", command=self.remove_home)
        self.remove_btn.grid(row=1, column=1, padx=5, pady=5)

        self.manage_btn = tk.Button(self.frame, text="Manage Home", command=self.manage_home)
        self.manage_btn.grid(row=1, column=2, padx=5, pady=5)

    def update_home_list(self):
        self.home_list.delete(0, tk.END)
        for home in self.smart_homes_app.get_all_homes().values():
            summary = home.get_status_summary()
            self.home_list.insert(tk.END, summary)

    def add_home(self):
        name = simpledialog.askstring("Add Home", "Enter home name:")
        if name:
            try:
                self.smart_homes_app.add_home(name)
                self.update_home_list()
            except Exception as e:
                messagebox.showerror("Error", str(e))

    def remove_home(self):
        selected = self.home_list.curselection()
        if not selected:
            messagebox.showwarning("No Selection", "Please select a home first")
            return

        home_name = self.home_list.get(selected[0]).split(' ')[0]
        self.smart_homes_app.remove_home(home_name)
        self.update_home_list()

    def manage_home(self):
        selected = self.home_list.curselection()
        if not selected:
            messagebox.showwarning("No Selection", "Please select a home first")
            return

        home_name = self.home_list.get(selected[0]).split(' ')[0]
        home = self.smart_homes_app.get_home(home_name)
        if home:
            self.current_home = home
            self.open_home_window()

    def open_home_window(self):
        if self.current_home_window:
            self.current_home_window.destroy()

        self.current_home_window = tk.Toplevel(self.root)
        self.current_home_window.title(f"Managing: {self.current_home.name}")

        # Create device management UI similar to original
        home_app = HomeManagementApp(self.current_home_window, self.current_home, self.update_callback)

    def update_callback(self):
        self.update_home_list()
        if self.current_home_window:
            self.current_home_window.destroy()
            self.open_home_window()

    def on_closing(self):
        self.smart_homes_app.save_to_csv()
        self.root.destroy()

class HomeManagementApp:
    def __init__(self, root, smart_home: SmartHome, update_callback):
        self.root = root
        self.smart_home = smart_home
        self.update_callback = update_callback

        # Create GUI elements
        self.frame = tk.Frame(root)
        self.frame.pack(padx=10, pady=10)

        self.device_frames = []
        self.create_device_widgets()

        # Control buttons
        self.control_frame = tk.Frame(root)
        self.control_frame.pack(pady=10)

        tk.Button(self.control_frame, text="Turn All On",
                  command=self.turn_all_on).grid(row=0, column=0, padx=5)
        tk.Button(self.control_frame, text="Turn All Off",
                  command=self.turn_all_off).grid(row=0, column=1, padx=5)
        tk.Button(self.control_frame, text="Add Device",
                  command=self.add_device_window).grid(row=0, column=2, padx=5)
        tk.Button(self.control_frame, text="Back",
                  command=self.close_window).grid(row=0, column=3, padx=5)

    def create_device_widgets(self):
        for widget in self.device_frames:
            widget.destroy()
        self.device_frames = []

        for i, device in enumerate(self.smart_home.devices):
            frame = tk.Frame(self.frame, borderwidth=2, relief="groove")
            frame.grid(row=i, column=0, pady=5, sticky="ew")
            self.device_frames.append(frame)

            info = tk.Label(frame, text=str(device), width=40, anchor="w")
            info.grid(row=0, column=0, padx=5)

            toggle_btn = tk.Button(frame, text="Toggle",
                                  command=lambda idx=i: self.toggle_device(idx))
            toggle_btn.grid(row=0, column=1, padx=5)

            edit_btn = tk.Button(frame, text="Edit",
                                command=lambda idx=i: self.edit_device(idx))
            edit_btn.grid(row=0, column=2, padx=5)

            delete_btn = tk.Button(frame, text="Delete",
                                  command=lambda idx=i: self.delete_device(idx))
            delete_btn.grid(row=0, column=3, padx=5)

    def refresh_display(self):
        self.create_device_widgets()
        if self.update_callback:
            self.update_callback()

    def toggle_device(self, index):
        try:
            self.smart_home.toggle_device(index)
            self.refresh_display()
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def turn_all_on(self):
        self.smart_home.switch_all(True)
        self.refresh_display()

    def turn_all_off(self):
        self.smart_home.switch_all(False)
        self.refresh_display()

    def delete_device(self, index):
        try:
            self.smart_home.remove_device(index)
            self.refresh_display()
        except Exception as e:
            messagebox.showerror("Error", str(e))

    def edit_device(self, index):
        device = self.smart_home.devices[index]
        edit_window = tk.Toplevel(self.root)

        tk.Label(edit_window, text="Edit Device").pack(pady=10)

        entries = {}
        if isinstance(device, SmartPlug):
            tk.Label(edit_window, text="Consumption Rate (0-150):").pack()
            entries['consumption_rate'] = tk.Entry(edit_window)
            entries['consumption_rate'].pack()
        elif isinstance(device, SmartHeater):
            tk.Label(edit_window, text="Setting (0-5):").pack()
            entries['setting'] = tk.Entry(edit_window)
            entries['setting'].pack()
        elif isinstance(device, SmartDoor):
            tk.Label(edit_window, text="Locked (True/False):").pack()
            entries['locked'] = tk.Entry(edit_window)
            entries['locked'].pack()

        def save_changes():
            try:
                kwargs = {}
                for key, entry in entries.items():
                    value = entry.get()
                    if key == 'consumption_rate' or key == 'setting':
                        kwargs[key] = int(value)
                    elif key == 'locked':
                        kwargs[key] = value.lower() == 'true'
                self.smart_home.update_option(index, **kwargs)
                self.refresh_display()
                edit_window.destroy()
            except Exception as e:
                messagebox.showerror("Invalid Input", str(e))

        tk.Button(edit_window, text="Save", command=save_changes).pack(pady=10)

    def add_device_window(self):
        add_window = tk.Toplevel(self.root)
        tk.Label(add_window, text="Add New Device").pack(pady=10)

        device_type = tk.StringVar()
        tk.Radiobutton(add_window, text="SmartPlug", variable=device_type,
                      value="SmartPlug").pack()
        tk.Radiobutton(add_window, text="SmartHeater", variable=device_type,
                      value="SmartHeater").pack()
        tk.Radiobutton(add_window, text="SmartDoor", variable=device_type,
                      value="SmartDoor").pack()

        def create_device():
            try:
                if device_type.get() == "SmartPlug":
                    self.smart_home.add_device(SmartPlug(45))
                elif device_type.get() == "SmartHeater":
                    self.smart_home.add_device(SmartHeater())
                elif device_type.get() == "SmartDoor":
                    self.smart_home.add_device(SmartDoor())
                self.refresh_display()
                add_window.destroy()
            except Exception as e:
                messagebox.showerror("Error", str(e))

        tk.Button(add_window, text="Add", command=create_device).pack(pady=10)

    def close_window(self):
        self.root.destroy()

if __name__ == "__main__":
    root = tk.Tk()
    app = SmartHomeManagerApp(root)
    root.protocol("WM_DELETE_WINDOW", app.on_closing)
    root.mainloop()
